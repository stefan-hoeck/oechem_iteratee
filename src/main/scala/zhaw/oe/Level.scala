package zhaw.oe

import scalaz._, Scalaz._

/** Describes the severity of a logging message.
  */
sealed abstract class Level (val level: Int) extends Ordered[Level] {
  def name = toString.toLowerCase
  override def compare (that: Level) = this.level compare that.level

  def log(msg: ⇒ String): Log = Log log (msg, this)
}

object Level {
  case object Trace extends Level (0)
  case object Debug extends Level (10)
  case object Info extends Level (100)
  case object Warning extends Level (1000)
  case object Error extends Level (10000)

  lazy val values = List[Level] (Trace, Debug, Info, Warning, Error)

  implicit val LevelEqual = Equal.equalA[Level]
}

// vim: set ts=2 sw=2 et:

