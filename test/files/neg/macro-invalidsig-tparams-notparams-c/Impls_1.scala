import scala.reflect.makro.{Context => Ctx}

object Impls {
  def foo[T: c.TypeTag, U: c.TypeTag, V](c: Ctx)(implicit V: c.TypeTag[V]): c.Expr[Unit] = {
    import c.mirror._
    println(implicitly[TypeTag[T]])
    println(implicitly[TypeTag[U]])
    println(V)
    Literal(Constant(()))
  }
}
