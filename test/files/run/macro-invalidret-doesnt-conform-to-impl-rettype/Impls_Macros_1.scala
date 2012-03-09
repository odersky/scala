import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo(c: Ctx): c.Expr[Int] = {
    import c.mirror._
    val body = Literal(Constant("42"))
    // should fail when we implement a runtime assertion for the Expr ctor
    // for now I just introduce a syntax error to make this test fail silently
    Expr[Int](body
  }
}

object Macros {
  def foo = macro Impls.foo
}