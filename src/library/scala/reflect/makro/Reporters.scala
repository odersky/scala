package scala.reflect.makro

trait Reporters {
  self: Context =>

  import mirror._

  /** For sending a message which should not be labeled as a warning/error,
   *  but also shouldn't require -verbose to be visible.
   */
  def echo(msg: String): Unit
  def echo(pos: Position, msg: String): Unit

  /** Informational messages, suppressed unless -verbose or force=true. */
  def info(pos: Position, msg: String, force: Boolean): Unit

  /** Warnings and errors. */
  def hasWarnings: Boolean
  def hasErrors: Boolean
  def warning(pos: Position, msg: String): Unit
  def error(pos: Position, msg: String): Unit
}