package zhaw.oe

import java.nio.ByteBuffer
import openeye.oechem._
import openeye.oeomega._
import scala.collection.immutable.{IndexedSeq ⇒ IxSq}
import scalaz._, Scalaz._, iteratee._, Iteratee._, effect._
import scalaz.CharSet.UTF8
import scalaz.std.indexedSeq._

trait MolIterFunctions {
  import MolIter.{OEInResource, OEOutResource}
  import logDisIO._

  private def molEnum(in: oemolistream): EnumIO[Mol] =
    new EnumeratorT[Mol,LogDisIO] {
      def apply[A] = _ mapCont { c ⇒ {
          val m = new Mol
          val hasRead = oechem.OEReadMolecule(in, m)

          if (hasRead) c(elInput(m)) >>== apply[A]
          else c(emptyInput)
        }
      }
    }

  def molIn(p: String): EnumIO[Mol] =
    iter.resourceEnum(molInStream(p), p)(molEnum)

  def molOut(p: String): IterIO[Mol,Unit] = 
    iter.resourceIter(molOutStream(p), p)(
      (m, o) ⇒ except(point(oechem.OEWriteMolecule(o, m)).void,
        t ⇒ s"Error when writing to file $p: $t"))

  def removeDublicates[F[_]:Monad]: EnumerateeT[IxSq[Mol],IxSq[Mol],F] = {
    def canon(m: Mol): State[Set[ByteBuffer],Option[Mol]] = {
      oechem.OEAssignAromaticFlags(m)
      val s = ByteBuffer.wrap(oechem.OECreateIsoSmiString(m) getBytes UTF8)

      State(ss ⇒ if (ss(s)) (ss, None) else (ss + s, Some(m)))
    }

    def sts(ms: IxSq[Mol]): State[Set[ByteBuffer],IxSq[Mol]] =
      ms traverseS canon map { _.flatten }

    iter.accumMap(sts)(Set.empty)
  }

  private def molInStream(p: String): LogDisIO[oemolistream] = 
    openStream(IO(new oemolistream), p)(o ⇒ IO(o.open(p)))

  private def molOutStream(p: String): LogDisIO[oemolostream] = 
    openStream(IO(new oemolostream), p)(o ⇒ IO(o.open(p)))

  private def openStream[A]
    (create: IO[A], p: String)
    (open: A ⇒ IO[Boolean]): LogDisIO[A] = for {
      _ ← debug(s"Opening: $p")
      a ← liftIO(create)
      b ← liftIO(open(a))
      r ← liftDis(b ? a.right[Errors] |
                      s"Unable to open file: $p".wrapNel.left)
    } yield r
}

trait MolIterInstances {
  implicit val OEInResource: Resource[oemolistream] =
    new Resource[oemolistream] {
      def close(o: oemolistream) = IO.putStrLn(s"Closed: $o") >> IO(o.close())
    }

  implicit val OEOutResource: Resource[oemolostream] =
    new Resource[oemolostream] {
      def close(o: oemolostream) = IO.putStrLn(s"Closed: $o") >> IO(o.close())
    }
}

object MolIter extends MolIterFunctions with MolIterInstances

// vim: set ts=2 sw=2 et:
