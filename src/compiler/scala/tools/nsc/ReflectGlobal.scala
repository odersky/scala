package scala.tools.nsc

import reporters.Reporter

/** A version of Global that uses reflection to get class
 *  infos, instead of reading class or source files.
 */
class ReflectGlobal(currentSettings: Settings, reporter: Reporter)
  extends Global(currentSettings, reporter) with reflect.runtime.SymbolTable with reflect.runtime.ClassLoaders {

  override def transformedType(sym: Symbol) =
    erasure.transformInfo(sym,
      uncurry.transformInfo(sym,
        refChecks.transformInfo(sym, sym.info)))

  // @xeno.by: how do I say that I want implementation of ClassLoaders
  // that comes from reflect.runtime.SymbolTable, not from Global?

  override def staticClass(fullname: String) =
    super.staticClass(fullname)

  override def staticModule(fullname: String) =
    super.staticModule(fullname)

}
