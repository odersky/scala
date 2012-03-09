package scala.reflect.makro
package runtime

trait Reporters {
  self: Context =>

  def echo(msg: String): Unit = mirror.reporter.echo(msg)

  def echo(pos: Position, msg: String): Unit = mirror.reporter.echo(pos, msg)

  def info(pos: Position, msg: String, force: Boolean): Unit = mirror.reporter.info(pos, msg, force)

  def hasWarnings: Boolean = mirror.reporter.hasErrors

  def hasErrors: Boolean = mirror.reporter.hasErrors

  def warning(pos: Position, msg: String): Unit = mirror.reporter.warning(pos, msg)

  def error(pos: Position, msg: String): Unit = mirror.reporter.error(pos, msg)
}