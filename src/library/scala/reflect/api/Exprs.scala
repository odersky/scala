/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait Exprs { self: Universe =>

  /** An expression tree tagged with its type */
  case class Expr[+T](tree: Tree) {
    def tpe: Type = tree.tpe
    // assert(tree.tpe <:< T.typeTag.tpe)
    // would like to check the above assertion, but Manifests need to be part of the universe first.
    // so we need to:
    //    1.move Manifest to reflect.api.Universe (leave alias in reflect)
    //    2.change implicit search so that Manifests of arbitary universes can be generated.
    def eval: T = ??? // todo: implement this
    lazy val value: T = eval
    override def toString = "Expr["+tpe+"]("+tree+")"
  }

  // would be great if in future this generated an Expr[Magic]
  // where Magic is a magic untyped type that propagates through the entire quasiquote
  // and turns off typechecking whenever it's involved
  // that'd allow us to splice trees into quasiquotes and still have these qqs to be partially typechecked
  // see some exploration of these ideas here: https://github.com/xeno-by/alphakeplerdemo
  implicit def tree2expr(tree: Tree): Expr[Nothing] = Expr[Nothing](tree)
  implicit def expr2tree(expr: Expr[_]): Tree = expr.tree

  // @xeno.by: good idea?
  implicit def trees2exprs(trees: List[Tree]): List[Expr[Nothing]] = trees map tree2expr
  implicit def exprs2trees(exprs: List[Expr[_]]): List[Tree] = exprs map expr2tree
}

