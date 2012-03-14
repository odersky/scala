package scala.tools.nsc
package symtab

trait ClassLoaders { self: SymbolTable =>

  def staticClass(fullname: String) =
    definitions.getRequiredClass(fullname)

  def staticModule(fullname: String) =
    definitions.getRequiredModule(fullname)

}
