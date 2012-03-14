/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait StandardDefinitions { self: Universe =>

  val definitions: AbsDefinitions

  abstract class AbsDefinitions {
    // packages
    def RootPackage: Symbol
    def RootClass: Symbol
    def EmptyPackage: Symbol
    def ScalaPackage: Symbol

    // top types
    def AnyClass   : Symbol
    def AnyValClass: Symbol
    def AnyRefClass: Symbol
    def ObjectClass: Symbol

    // bottom types
    def NullClass   : Symbol
    def NothingClass: Symbol

    // the scala value classes
    def UnitClass   : Symbol
    def ByteClass   : Symbol
    def ShortClass  : Symbol
    def CharClass   : Symbol
    def IntClass    : Symbol
    def LongClass   : Symbol
    def FloatClass  : Symbol
    def DoubleClass : Symbol
    def BooleanClass: Symbol

    // fundamental reference classes
    def ScalaObjectClass : Symbol
    def SymbolClass : Symbol
    def StringClass : Symbol
    def ClassClass  : Symbol

    // product, tuple, function
    def TupleClass    : Array[Symbol]
    def ProductClass  : Array[Symbol]
    def FunctionClass : Array[Symbol]

    // Option classes
    def OptionClass: Symbol
    def SomeClass: Symbol
    def NoneModule: Symbol
    def SomeModule: Symbol

    // collections classes
    def ConsClass: Symbol
    def IterableClass: Symbol
    def IteratorClass: Symbol
    def ListClass: Symbol
    def SeqClass: Symbol
    def StringBuilderClass: Symbol
    def TraversableClass: Symbol

    // collections modules
    def PredefModule: Symbol
    def ListModule: Symbol
    def List_apply: Symbol
    def NilModule: Symbol
    def SeqModule: Symbol
    def IteratorModule: Symbol
    def Iterator_apply: Symbol

    // arrays and their members
    def ArrayModule: Symbol
    def ArrayModule_overloadedApply: Symbol
    def ArrayClass: Symbol
    def Array_apply: Symbol
    def Array_update: Symbol
    def Array_length: Symbol
    def Array_clone: Symbol

    // special parameter types
    def ByNameParamClass: Symbol
    def JavaRepeatedParamClass: Symbol
    def RepeatedParamClass: Symbol

    /** Given a type T, returns the type corresponding to the VM's
     *  representation: ClassClass's type constructor applied to `arg`.
     */
    def vmClassType(arg: Type): Type    // !!! better name?

    /** The string representation used by the given type in the VM.
     */
    def vmSignature(sym: Symbol, info: Type): String

    /** Is symbol one of the value classes? */
    def isValueClass(sym: Symbol): Boolean        // !!! better name?

    /** Is symbol one of the numeric value classes? */
    def isNumericValueClass(sym: Symbol): Boolean   // !!! better name?
  }
}
