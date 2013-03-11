package zhaw.oe

/** A basic logging message whose string message is lazily evaluated
  */
sealed trait Log {
  def msg: String 
  def level: Level
}

trait LogFunctions {
  import Level._

  def log (m: ⇒ String, lvl: Level): Log = new Log {
    lazy val msg = m
    def level = lvl
  }

  def trace (msg: ⇒ String) = log (msg, Trace)

  def debug (msg: ⇒ String) = log (msg, Debug)

  def info (msg: ⇒ String) = log (msg, Info)

  def warning (msg: ⇒ String) = log (msg, Warning)

  def error (msg: ⇒ String) = log (msg, Error)
}

object Log  extends LogFunctions

// vim: set ts=2 sw=2 et:
