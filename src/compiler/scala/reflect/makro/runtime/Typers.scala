package scala.reflect.makro
package runtime

trait Typers {
  self: Context =>

  val callsiteTyper: mirror.analyzer.Typer

  def typeCheck(tree: Tree, pt: Option[Type] = None): Option[Tree] = {
    def trace(msg: Any) = if (mirror.settings.Ymacrotyperdebug.value) println(msg)
    trace("typechecking %s with expected type %s".format(tree, pt))
    callsiteTyper.context.withImplicitsEnabled(callsiteTyper.silent(_.typed(tree, mirror.analyzer.EXPRmode, pt getOrElse mirror.WildcardType)) match {
      case mirror.analyzer.SilentResultValue(result) =>
        trace(result)
        Some(result)
      case error @ mirror.analyzer.SilentTypeError(_) =>
        trace(error.err.errMsg)
        None
    })
  }

  type TypeError = mirror.TypeError

  object TypeError extends TypeErrorExtractor {
    def unapply(error: TypeError): Option[(Position, String)] = Some((error.pos, error.msg))
  }

  def resetAllAttrs[T <: Tree](x: T): T = mirror.resetAllAttrs(x)

  def resetLocalAttrs[T <: Tree](x: T): T = mirror.resetLocalAttrs(x)
}