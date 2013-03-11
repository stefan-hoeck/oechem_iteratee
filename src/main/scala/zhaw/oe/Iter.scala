package zhaw.oe

import scalaz.{Reader ⇒ _, Writer ⇒ _, _}, Scalaz._, std.indexedSeq._
import scalaz.iteratee._, Iteratee._
import scalaz.effect._

trait IterFunctions {
  import iter.EffectStep, logDisIO._

  /** Helper function to create cont-Steps */
  private[oe] def contI[E,A](el: E ⇒ EffectStep[E,A], eof: LogDisIO[A])
    : EffectStep[E,A] =
    success[StepIO[E,A]](
      scont[E,LogDisIO,A] { i ⇒ 
        vIter[E,A] (
          i.fold[EffectStep[E,A]] (
            empty = contI(el, eof),
            el    = el(_),
            eof   = eof map { sdone(_, eofInput) }
          )
        )
      }
    )


  /** Map over elements while accumulating state
    *
    * Can be used for instance to implement duplicate filtering
    */
  def accumMap[E,F[_]:Monad,S](p: E ⇒ State[S,E])(s: S)
    : EnumerateeT[E, E, F] = new EnumerateeT[E, E, F] {
    def apply[A] = {
      def loop(s: S) = step(s) andThen cont[E, F, StepT[E, F, A]]
      def step(s: S): (Input[E] ⇒ IterateeT[E, F, A]) ⇒ 
                      (Input[E] ⇒ IterateeT[E, F, StepT[E, F, A]]) = {
        k ⇒ in ⇒
          in(
            el = e ⇒ {
              val (newS, newE) = p(e) apply s
              k(elInput(newE)) >>== doneOr(loop(newS))
            }
            , empty = cont(step(s)(k))
            , eof = done(scont(k), in)
          )
      }

      EnumerateeT.doneOr(loop(s))
    }
  }

  /** Returns an Iteratee that feeds values to a resource R.
    *
    * The resource is automatically closed, when no more values
    * are available (upon EOF input), and if output to the
    * resource fails (that is, param `out` returns a left).
    * Enumerators must therefore make sure, that if they themselves
    * produce erroneous input, they at least call the iteratee
    * with EOF before returning the failure. See [[efa.io.RecursiveEnumIO]]
    * for an example.
    *
    * @tparam E the element type to be consumed
    * @tparam R the type of the resource
    * @param  create an IO action that creates a resource of type R
    * @param  name a `String` representation of the resource
    *              (a file path for instance) used for logging
    *              and error messages
    * @param  out writes an `E` to a resource of type `R`. This
    *             action might fail, in which case the resource
    *             is closed and no more values are accepted.
    */
  def resourceIter[E,R:Resource]
    (create: LogDisIO[R], name: String)
    (out: (E,R) ⇒ LogDisIO[Unit]): IterIO[E,Unit] = {
      def ex(e: E, r: R): EffectStep[E,Unit] =
        onFail(out(e, r) >> goR(r), close(r, name))

      def goR(r: R): EffectStep[E,Unit] = contI(ex(_, r), close(r, name))

      def go: EffectStep[E,Unit] = 
        contI(e ⇒ create >>= { ex(e, _) }, ldiUnit)
      
      vIter(go)
    }

  /** Creates an enumerator for a closable resource, guaranteeing
    * that the resource is released when it is no longer needed.
    *
    * The resulting enumerator will open (create) the resource if
    * required, create an underlying enumerator from it, and return
    * all of its elements. If no more elements are available or
    * needed, the resource is closed automatically.
    *
    * @tparam E the element type of the enumerator
    * @tparam R the type of the resource
    * @param  r an IO action that creates a resource of type R
    * @param  name a `String` representation of the resource
    *              (a file path for instance) used for logging
    *              and error messages
    * @param  enum creates an (unmanaged) enumerator from the resource
    */
  def resourceEnum[E,R:Resource]
    (r: LogDisIO[R], name: String)
    (enum: R ⇒ EnumIO[E]): EnumIO[E] = new EnumeratorT[E,LogDisIO] {
      def apply[A] = (s: StepIO[E,A]) ⇒ {
        def step: EffectStep[E,A] =
          r >>= { x ⇒ ensure(enum(x) apply s value, close(x, name)) }

        //open resource only if s is not in 'done' state already
        s mapCont { _ ⇒ vIter(step) } 
      }
    }

  /** Counts elements together with the time (in ms) used
    * to accumulate them and performs an IO action with this
    * data at regular intervals.
    *
    * Iteratees like this one can be used to regularely
    * update a progress bar in a user interface, or print
    * logging messages about the progress of a lengthy procedure
    * to the console.
    *
    * @tparam E the element type the throbber consumes
    * @param inc the number of elements to be processed
    *            between each IO action
    * @param out an IO action that consumes the number of
    *            elements accumulated so far plus the
    *            time taken (in milliseconds) to do so
    */
  def throbber[E](inc: Int, out: (Int, Long) ⇒ IO[Unit])
    : IterateeT[E,IO,Unit] = {
    type ToIter = Input[E] ⇒ IterateeT[E,IO,Unit]

    def now = System.currentTimeMillis
    def cont(t: ToIter) = IO(scont(t))
    def icont(t: ToIter) = iterateeT(cont(t))
    def report(acc: Int, millis: Long) =
      IO putStrLn s"Accumulated $acc items in $millis ms"

    def step(acc: Int, cnt: Int, start: Long): ToIter = i ⇒ 
      (acc, cnt, i) match {
        case (a, 0, Input.Element(_)) ⇒ 
          iterateeT(report(a + inc, now - start) >>
          cont(step(a + inc, inc - 1, start)))
        case (a, x, Input.Element(_)) ⇒ icont(step(a, x - 1, start))
        case (a, x, Input.Empty())    ⇒ icont(step(a, x, start))
        case (a, x, Input.Eof())      ⇒ iterateeT(IO(sdone((), eofInput)))
      }

    icont(step(0, inc - 1, now))
  }

  /** A throbber that outputs its accumulated information
    * to a [[efa.io.Logger]].
    *
    * @tparam E the element type the throbber consumes
    * @param inc the number of elements to be processed
    *            between each IO action
    * @param logger the [[efa.io.Logger]] used to process the
    *               throbber's messages
    * @param lbl the logging level used for the messages
    * @param msg a function to compose the logging message from
    *            the number of processed elements and the time
    *            (in ms) take to do so
    */
  def logThrobber[E](
    inc: Int,
    logger: Logger = Logger.consoleLogger,
    lvl: Level = Level.Info,
    msg: (Int, Long) ⇒ String)
  : IterateeT[E,IO,Unit] = 
    throbber[E](inc, (i,l) ⇒ logger log lvl.log(msg(i,l)))

  /** Helper function to create iteratees */
  private[oe] def vIter[E,A](s: EffectStep[E,A]): IterIO[E,A] =
    iterateeT[E,LogDisIO,A](s)

  private[oe] def doneIter[E,A](a: ⇒ A): IterIO[E,A] =
    vIter[E,A](success(sdone(a, emptyInput)))
}

trait IterInstances {
  /** Monoid implementation for Iteratees
    *
    * This implementation will pass input to both appended Iteratees
    * until both are in Done state at which point the appended results
    * of both Iteratees will be returned. This is useful for concatenating
    * several data sinks. One could for instance ouput stuff to the console
    * as well as a file for instance.
    *
    * One implementation detail: This function could have been implemented
    * via function `zip` defined on `IterateeT` and then using map and
    * Monoid `append` to concatenate the zipped results. However, this
    * implementation is more memory efficient.
    */
  implicit def IterMonoid[E,F[_]:Monad,A:Monoid]: Monoid[IterateeT[E,F,A]] =
    new Monoid[IterateeT[E,F,A]] {
      type Iter[X] = IterateeT[E,F,X]
      type Stp[X] = StepT[E,F,X]

      val zero = ∅[A].η[Iter]

      def append(a: Iter[A], b: ⇒ Iter[A]): Iter[A] = iterateeT[E,F,A](
        for {
          sta ← a.value
          stb ← b.value
        } yield stepConcat(sta, stb)
      )

      import StepT.{Cont, Done}
      def stepConcat(a: Stp[A], b: Stp[A]): Stp[A] = (a, b) match {
        case (Cont(fa), Cont(fb))     ⇒ scont(i ⇒ append(fa(i), fb(i)))
        case (Cont(fa), b@Done(_, _)) ⇒ scont(i ⇒ append(fa(i), b.pointI))
        case (a@Done(_, _), Cont(fb)) ⇒ scont(i ⇒ append(a.pointI, fb(i)))
        case (Done(a, i), Done(b, j)) ⇒ sdone(a ⊹ b, i)
      }
    }
}

object iter extends IterFunctions with IterInstances {
  type EffectStep[E,A] = LogDisIO[StepIO[E,A]]
}

// vim: set ts=2 sw=2 et:
