package scala.tools.nsc
package typechecker

import collection.mutable
import reflect.internal.Flags._

trait LateDefinitions { self: Global =>

  /** A transient flag to mark a val or def haswith an inferred result type
   *  Reset by Typer.
   */
  final val INFERRED = TRANS_FLAG_2

  /** Maps symbols to the definitions they generate.
   *  The maps are populated in Namers and bindings are removed again in Typers.
   */
  class LateDefs {
    /** Maps symbols to the late trees that define them */
    private val lateDefs = mutable.Map[Symbol, Tree]()

    /** Maps symbols to the symbols that depend on them (e.g. field -> getter/setter,
     *  method -> default argument, class -> companion object) */
    private val dependentSymbols = new mutable.HashMap[Symbol, List[Symbol]] {
      override def default(key: Symbol) = List()
    }

    /** Clear all maps */
    def clear() {
      lateDefs.clear()
      dependentSymbols.clear()
    }

    /** Enter a tree into maps
     *  @param  tree    The tree to be inserted at Typers. This must be a definition with a defined symbol.
     *  @param original If the tree was created as a supplementary method for some original definition,
     *                  the symbol of the original definition. Otherwise NoSymbol.
     *                  Dependent definitions will be inserted after their original definitions.
     *                  Original symbols that do not share the same owner with the tree's symbol are ignored.
     */
    def enter(tree: Tree, original: Symbol = NoSymbol): Unit = {
      assert(tree.symbol != NoSymbol)
      lateDefs(tree.symbol) = tree
      if (original != NoSymbol && original.owner == tree.symbol.owner) {
        dependentSymbols(original) :+= tree.symbol
      }
    }

    /** Remove symbol from maps, and return its associated tree and dependent symbols */
    def remove(sym: Symbol): (Tree, List[Symbol]) =
      (lateDefs remove sym getOrElse EmptyTree, dependentSymbols remove sym getOrElse Nil)

  }

}
