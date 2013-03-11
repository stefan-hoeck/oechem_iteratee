package zhaw.oe

import scalaz._, Scalaz._, effect._, scalaz.iteratee.IterateeT

/** Helper functions for working in the LogDisIO-Monad */
trait LogDisIOFunctions {
  val logM = Monad[LogDisIO]

  val ldiUnit: LogDisIO[Unit] = point(())

  def point[A](a: ⇒ A): LogDisIO[A] = logM point a

  def lift[A](a: Logger ⇒ IO[DisRes[A]]): LogDisIO[A] =
    Kleisli[DisIO,Logger,A](l ⇒ EitherT(a(l)))

  def liftIO[A](i: IO[A]): LogDisIO[A] = liftDisIO(i map { _.right })

  def liftLogIO[A](i: Logger ⇒ IO[A]): LogDisIO[A] = 
    lift(i andThen { _ map { _.right } })

  def liftDisIO[A](i: IO[DisRes[A]]): LogDisIO[A] = lift(_ ⇒ i)

  def liftDis[A](i: DisRes[A]): LogDisIO[A] = liftDisIO(IO(i))

  def mapIO[A](a: LogDisIO[A])(f: IO[DisRes[A]] ⇒ IO[DisRes[A]])
    : LogDisIO[A] = lift(l ⇒ f(a run l run))

  /** Tries to carry out the given IO action and - in case of
    * a failure, returns an error message wrapped in a left.
    */
  def except[A](e: LogDisIO[A], msg: Throwable ⇒ String): LogDisIO[A] =
    mapIO(e)(_ except (t ⇒ IO(msg(t).wrapNel.left[A])))

  /** Runs the first IO-action and in case of a failure,
    * also the second.
    */
  def onFail[A,B](e: LogDisIO[A], ex: LogDisIO[B]): LogDisIO[A] = {
    def onEx(l: Logger): IO[DisRes[A]] = for {
      v ← e.run(l).run
      _ ← v fold (_ ⇒ ex as (), (a: A) ⇒ ldiUnit) run l run
    } yield v

    lift(onEx)
  }

  def ensure[A](e: LogDisIO[A], f: LogDisIO[Unit]): LogDisIO[A] = {
    def ens(l: Logger): IO[DisRes[A]] = for {
      v ← e.run(l).run onException f.run(l).run
      _ ← f.run(l)run
    } yield v

    lift(ens)
  }

  def log(msg: ⇒ String, level: Level): LogDisIO[Unit] =
    lift { _ log level.log(msg) map (_.right) }

  def trace(msg: ⇒ String) = log(msg, Level.Trace)

  def debug(msg: ⇒ String) = log(msg, Level.Debug)

  def info(msg: ⇒ String) = log(msg, Level.Info)

  def warning(msg: ⇒ String) = log(msg, Level.Warning)

  def error(msg: ⇒ String) = log(msg, Level.Error)

  def fail[A](s: ⇒ String): LogDisIO[A] = failNel(s.wrapNel)

  def failNel[A](s: ⇒ NonEmptyList[String]): LogDisIO[A] =
    liftDisIO(IO(s.left))

  def failIter[E,A](s: ⇒ String): IterIO[E,A] = failNelIter(s.wrapNel)

  def failNelIter[E,A](s: ⇒ NonEmptyList[String]): IterIO[E,A] =
    IterateeT.iterateeT[E,LogDisIO,A](failNel(s))
      
  def success[A](a: ⇒ A): LogDisIO[A] = point(a)

  /**
    * Closes a resource
    */
  def close[A:Resource](c: A, name: String): LogDisIO[Unit] = {
    def cl: IO[Unit] = Resource[A] close c except (_ ⇒ IO.ioUnit)

    liftIO(cl) >> debug(s"Closed $name")
  }
}

trait LogDisIOInstances {
  implicit def LogDisIOMonoid[A:Monoid] = Monoid.liftMonoid[LogDisIO,A]
}

object logDisIO extends LogDisIOFunctions with LogDisIOInstances
