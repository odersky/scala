package scala.reflect
package api

import scala.reflect.makro.Context

object Reifiers {
  def reify[T](cc: Context)(prefix: cc.Tree)(expr: cc.Tree): cc.Tree = {
    import cc.mirror._

    val reified = cc.reifyTree(expr)

    val Block(mirrorDecl @ ValDef(_, mr, _, _) :: symbolTable, unwrapped) = reified
    val rewrapped = Block(symbolTable, unwrapped)

    new Transformer {
      override def transform(tree: Tree) = tree match {
        case Select(ident @ Ident(_), name) if ident.name == mr =>
          Select(prefix, name)
        case _ =>
          super.transform(tree)
      }
    }.transform(rewrapped)
  }
}
