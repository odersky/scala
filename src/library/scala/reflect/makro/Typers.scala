package scala.reflect.makro

trait Typers {
  self: Context =>

  import mirror._

  /** Typechecks the provided tree against the expected type ``pt'' in the macro callsite context.
   *  Throws a ``TypeError'' if something goes wrong.
   */
  def typeCheck(tree: Tree, pt: Option[Type] = None): Option[Tree]

  /** Recursively resets symbols and types in a given tree.
   *
   *  Note that this does not revert the tree to its pre-typer shape.
   *  For more info, read up https://issues.scala-lang.org/browse/SI-5464.
   */
  def resetAllAttrs[T <: Tree](x: T): T

  /** Recursively resets locally defined symbols and types in a given tree.
   *
   *  Note that this does not revert the tree to its pre-typer shape.
   *  For more info, read up https://issues.scala-lang.org/browse/SI-5464.
   */
  def resetLocalAttrs[T <: Tree](x: T): T

  /** Represents an error during typechecking
   */
  type TypeError <: Throwable
  val TypeError: TypeErrorExtractor
  abstract class TypeErrorExtractor {
    def unapply(error: TypeError): Option[(Position, String)]
  }
}