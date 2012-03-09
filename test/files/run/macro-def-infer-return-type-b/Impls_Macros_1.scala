import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[T](c: Ctx)(x: c.Expr[T]) = ???
}

object Macros {
  def foo[T](x: T) = macro Impls.foo[T]
}