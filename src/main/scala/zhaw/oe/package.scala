package zhaw

import collection.immutable.{IndexedSeq ⇒ IxSq}
import scalaz._, Scalaz._, effect.IO, iteratee._

package object oe {
  type Nel[+A] = NonEmptyList[A]

  type Mol = openeye.oechem.OEGraphMol

  type Mols = IxSq[Mol]

  type Errors = Nel[String]

  type DisIO[+A] = EitherT[IO,Errors,A]

  type LogDisIO[A] = Kleisli[DisIO,Logger,A]

  type EnumIO[A] = EnumeratorT[A,LogDisIO]

  type IterIO[E,A] = IterateeT[E,LogDisIO,A]

  type StepIO[E,A] = StepT[E,LogDisIO,A]

  type DisRes[+A] = Errors \/ A
}

// vim: set ts=2 sw=2 et:
