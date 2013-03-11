package zhaw.oe

import scalaz._, Scalaz._, effect.{IO, SafeApp}
import scalaz.iteratee._, Iteratee._
import scalaz.std.indexedSeq._

object Main extends SafeApp with MolIterFunctions {
  val logger = Logger.consoleLogger

  /** Copy a file
    *
    * Args: First arg path of input file, second arg path of output file
    */
//  override def runl(args: List[String]): IO[Unit] = args match {
//    case in :: out :: Nil ⇒ logger logDisZ (molOut(out) &= molIn(in) run)
//    case _                ⇒ IO putStrLn s"Missing input and output path"
//  }

  /** Merging files
    *
    * Args: First arg path of output file, then an arbitrary number of
    * input files
    */
//  override def runl(args: List[String]): IO[Unit] = args match {
//    case out :: ins ⇒ 
//      logger logDisZ (molOut(out) &= (ins foldMap molIn) run)
//    case _          ⇒ IO putStrLn s"Missing output path"
//  }

  /** Remove duplicates
    *
    * Args: First arg path of output file, then an arbitrary number of
    * input files
    */
//  override def runl(args: List[String]): IO[Unit] = args match {
//    case out :: ins ⇒ logger logDisZ mergeNoDs(ins, out)
//    case _          ⇒ IO putStrLn s"Missing output path"
//  }

  def mergeNoDs(ins: List[String], out: String): LogDisIO[Unit] = {
    def remD = removeDublicates[LogDisIO]

    def grp = group[Mol,IxSq,LogDisIO](1000)

    def flatten = flatMap[Mols,Mol,LogDisIO](ms ⇒ enumList(ms.toList))

    molOut(out) &= (ins foldMap molIn mapE grp mapE remD mapE flatten) run
  }

}

// vim: set ts=2 sw=2 et:
