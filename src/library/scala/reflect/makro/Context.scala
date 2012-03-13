package scala.reflect.makro

trait Context extends Aliases
              with Reifiers
              with Reporters
              with Symbols
              with Typers
              with Utils {

  /** The mirror that corresponds to the compile-time universe */
  val mirror: scala.reflect.api.Universe

  /** The type of the prefix tree from which the macro is selected */
  type PrefixType

  /** The prefix tree from which the macro is selected */
  val prefix: Expr[PrefixType]

  /** Alias to the underlying mirror's reify */
  def reify[T](expr: T): Expr[T] = macro Context.reify[T]
}

object Context {
  def reify[T](cc: Context{ type PrefixType = Context })(expr: cc.Expr[T]): cc.Expr[cc.prefix.value.Expr[T]] = {
    import cc.mirror._
    val prefix = Select(cc.prefix, newTermName("mirror"))
    scala.reflect.api.Reifiers.reify[T](cc)(prefix)(expr)
  }
}
