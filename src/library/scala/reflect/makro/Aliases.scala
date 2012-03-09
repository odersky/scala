package scala.reflect.makro

trait Aliases {
  self: Context =>

  /** Aliases of mirror types */
  type Symbol = mirror.Symbol
  type Type = mirror.Type
  type Name = mirror.Name
  type Tree = mirror.Tree
  type Position = mirror.Position
  type Scope = mirror.Scope
  type Modifiers = mirror.Modifiers
  type Expr[+T] = mirror.Expr[T]
  type TypeTag[T] = mirror.TypeTag[T]

  /** Creator/extractor objects for Expr and TypeTag values */
  val TypeTag = mirror.TypeTag
  def typeTag[T](implicit typeTag: TypeTag[T]) = typeTag
  val Expr = mirror.Expr
}
