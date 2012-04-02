package scala.tools.nsc
package typechecker

import symtab.Flags._
import scala.collection.{ mutable, immutable }
import scala.tools.util.StringOps.{ ojoin }

trait Derivers { this: Analyzer =>

  import global._
  import definitions._

  trait Deriver { self: NewNamer =>

    import NamerErrorGen._

    /** A helper class that simplifies the creation of definitions that are
     *  in some way derived from an `original` definition.
     *  The main value that needs to be defined in subclasses is `derivedTree`,
     *  which returns the unattributed tree of the derived definition.
     *  The main method called by clients of this class is `enterLateDef`,
     *  which enters the definition into the typechecking process and
     *  returns `derivedTree` with a symbol representing the definition.
     */
    abstract class DefGen(original: MemberDef) {

      /** The name of the derived method */
      def name: TermName

      /** Which meta-annotation is associated with this kind of entity.
       *  Presently one of: field, getter, setter, beanGetter, beanSetter, param,
       *  companionClass, companionObject, companionMethod.
       */
      def annotCategory: Symbol

      /** Should annotations whose definitions are not meta-annotated be kept? */
      def keepCleanAnnots: Boolean = false

      /** The flags that are retained from the original symbol */
      def flagsMask: Long

      /** The flags that the derived symbol has in addition to those retained from
       *  the original symbol*/
      def flagsExtra: Long

      /** The flags of the derived definition; may be overridden in subclasses */
      def derivedFlags = original.mods.flags & flagsMask | flagsExtra

      /** The modifiers of the derived definition; may be overridden in subclasses */
      def derivedMods = Modifiers(derivedFlags, original.mods.privateWithin)

      /** The definition tree of the derived symbol.
       *  If it's a lazy val, it will be forced by enterLateDef.
       */
      val derivedTree: MemberDef

      /** The symbol of the derived tree; valid after enterLateDef has been called */
      def derivedSym: Symbol =
        derivedTree.symbol orElse {
          throw new AssertionError("can't get a derivedSym before enterLateDef is called")
        }

      /** A hook for typer-time validations of this definition, called from postProcess */
      def validate(): Unit = {}

      /** A postprocessing operation which is invoked during Typers at the time where the
       *  tree is inserted into the enclosing statement sequence. By default this does
       *  validation and annotation processing
       */
      def postProcess(tree: Tree): Tree = {
        validate()
        derivedSym setAnnotations deriveAnnotations(original, annotCategory, keepCleanAnnots)
        logDerived(tree)
      }

      private def logDerived(result: Tree): Tree = {
       debuglog("[+derived] " + ojoin(result.symbol.accurateKindString, result.symbol.getterName.decode)
         + " (" + derivedSym + ")\n        " + result)
       result
     }

     /** Creates a late definition; creates and enters its symbol, returns it.
      *  Sets original.symbol to newly created symbol unless it's already defined.
      */
     def enterLateDef(): MemberDef = {
        val sym = self.enterLateDef(derivedTree, original.symbol, postProcess)
        if (original.symbol == NoSymbol) original.symbol = sym
        derivedTree
      }
    }

    /** The annotations amongst those found on the original symbol which
     * should be propagated to this kind of accessor.
     */
    private def deriveAnnotations(original: Tree, annotCategory: Symbol, keepCleanAnnots: Boolean): List[AnnotationInfo] = {
      original.symbol.initialize.annotations filter { ann =>
        // There are no meta-annotation arguments attached to `ann`
        if (ann.metaAnnotations.isEmpty) {
          // A meta-annotation matching `annotCategory` exists on `ann`'s definition.
          (ann.defaultTargets contains annotCategory) ||
          // `ann`'s definition has no meta-annotations, and `keepCleanAnnots` is true.
          (ann.defaultTargets.isEmpty && keepCleanAnnots)
        }
        // There are meta-annotation arguments, and one of them matches `annotCategory`
        else ann.metaAnnotations exists (_ matches annotCategory)
      }
    }

    /** Enters an implicit conversion method for an implicit class */
    def enterImplicitWrapper(original: ClassDef) = {
      class ImplicitClassWrapperGen extends DefGen(original) {
        def flagsMask = AccessFlags
        def flagsExtra = METHOD | IMPLICIT
        def name = nme.implicitWrapperName(original.name)
        def annotCategory = MethodTargetClass
        lazy val derivedTree = factoryMeth(derivedMods, name, original)
      }
      new ImplicitClassWrapperGen().enterLateDef()
    }

    /** Enters all definitions (setters/getters/fields) associated with a val or var.
     *
     *  The architecture of that method is somewhat special: It's a long method
     *  where value definitions are interspersed with embedded classes.
     *
     *  This tells a story: How fields, then getters, then setters, then beans are
     *  defined. Every step may refer to symbols defined in the previous steps.
     *  The embedded classes appear at a point where everything before them
     *  is defined and everything after them is off limits. The embedded
     *  classes profit from systematic code reuse by inheriting from each other
     *  and the common superclass derivedDef.
     *
     *  So we have two axes of references here: Inherited ones which capture
     *  commonality of field and accessor definitions, and scoped ones, which
     *  capture the flow of definitions.
     *
     *  The 2.9 version of this complex captured flow but was bad at reuse because all accessors
     *  were defined directly in long methods. The previous 2.10 version captured
     *  reuse well but it was very hard to see what got defined when and was available
     *  to whom. The new design here captured both dimensions equally well.
     */
    def enterAllValDef(original: ValDef) = {
      val origMods = original.mods

      // preliminary checks
      if (origMods.isPrivateLocal)
        PrivateThisCaseClassParameterError(original)
      if (nme.isSetterName(original.name))
        ValOrValWithSetterSuffixError(original)

      // generate the field
      class FieldGen extends DefGen(original) {
        def flagsMask = FieldFlags
        def flagsExtra = PrivateLocal
        def name = nme.getterToLocal(original.name)
        def annotCategory = FieldTargetClass
        // By default annotations go to the field, except if the field is
        // generated for a class parameter (PARAMACCESSOR).
        override def keepCleanAnnots = !origMods.isParamAccessor
        lazy val derivedTree = atPos(original.pos) {
          ValDef(derivedMods, name, original.tpt, original.rhs)
        }
      }

      val field = if (origMods.isDeferred) EmptyTree else new FieldGen().enterLateDef()

      // utility methods for subsequent steps
      def fieldSelection =
        if (field.symbol.owner.isTerm) Ident(field.symbol)
        else Select(This(field.symbol.owner), field.symbol)

      def setterParamName = nme.syntheticParamName(1)
      def initialization = Assign(fieldSelection, Ident(setterParamName))

      // generate the getter
      class GetterGen extends DefGen(original) {
        def flagsMask  = GetterFlags
        def flagsExtra = ACCESSOR | ( if (origMods.isMutable) 0 else STABLE )
        def name = nme.getterName(original.name)
        def annotCategory = GetterTargetClass

        /** Check that any BeanProperty annotations appear under their proper name,
         *  and that no annotation named BeanProperty means something else.
         *  This is necessary because beans are created before annotations can
         *  be name-resolved.
         */
        private def validateBeanAnnot(beanAnnot: Symbol) {
          if ((original.symbol hasAnnotation beanAnnot) !=
              (original.mods hasAnnotationNamed beanAnnot.name.toTypeName))
            BeanPropertyAnnotationLimitationError(original)
        }
        override def validate() {
          if (derivedSym.isOverloaded) GetterDefinedTwiceError(derivedSym)
          validateBeanAnnot(BeanPropertyAttr)
          validateBeanAnnot(BooleanBeanPropertyAttr)
          super.validate()
        }

        def vparamss: List[List[ValDef]] = Nil
        val (tpt, rhs, pos) =
          if (origMods.isDeferred)
            (original.tpt, EmptyTree, original.pos)
          else {
            // For existentials, don't specify a type for the getter, even one derived
            // from the symbol! This leads to incompatible existentials for the field and
            // the getter. Let the typer do all the work. You might think "why only for
            // existentials, why not always," and you would be right, except: a single test
            // fails, but it looked like some work to deal with it. Test neg/t0606.scala
            // starts compiling (instead of failing like it's supposed to) because the typer
            // expects to be able to identify escaping locals in typedDefDef, and fails to
            // spot that brand of them. In other words it's an artifact of the implementation.
            val duptpt = field.symbol.tpe match {
              case ExistentialType(_, _) => TypeTree()
              case tp => TypeTree(tp)
            }
            var init = gen.mkCheckInit(fieldSelection)
            if (original.mods.isLazy) init = Block(List(initialization), init)
            (duptpt, init, original.pos.focus)
          }
        lazy val derivedTree = atPos(pos) {
          DefDef(derivedMods, name, Nil, vparamss, tpt, rhs)
        }
      }

      val getter = new GetterGen().enterLateDef()

      // generate the setter
      class SetterGen extends DefGen(original) {
        def flagsMask = SetterFlags
        def flagsExtra = ACCESSOR
        def name = nme.getterToSetter(getter.name)
        def annotCategory = SetterTargetClass
        val setterParam = ValDef(
          Modifiers(PARAM), setterParamName, TypeTree(getter.symbol.tpe.finalResultType), EmptyTree)
        lazy val derivedTree = atPos(original.pos.focus) {
          DefDef(derivedMods, name, Nil, List(List(setterParam)), TypeTree(UnitClass.tpe),
          if (original.mods.isDeferred) EmptyTree else initialization)
        }
      }

      if (original.mods.isMutable) new SetterGen().enterLateDef()

      // generate any bean accessors
      trait BeanGen extends DefGen {
        protected def prefix: String
        override def flagsMask  = BeanPropertyFlags
        override def flagsExtra = 0
        override def name = newTermName(prefix + original.name.toString.capitalize)
      }

      class BeanGetterGen(val prefix: String) extends GetterGen with BeanGen {
        override def annotCategory = BeanGetterTargetClass
        override def vparamss: List[List[ValDef]] = List(Nil)
      }

      class BeanSetterGen extends SetterGen with BeanGen {
        def prefix = "set"
        override def annotCategory = BeanSetterTargetClass
      }

      def makeBeans(prefix: String): Unit = {
        if (!original.name(0).isLetter)
          BeanPropertyAnnotationFieldWithoutLetterError(original)
        else if (original.mods.isPrivate)  // avoids name clashes with private fields in traits
          BeanPropertyAnnotationPrivateFieldError(original)
        new BeanGetterGen(prefix).enterLateDef()
        if (original.mods.isMutable) new BeanSetterGen().enterLateDef()
      }

      if (!forMSIL)
        if (original.mods hasAnnotationNamed tpnme.BeanPropertyAnnot) makeBeans("get")
        else if (original.mods hasAnnotationNamed tpnme.BooleanBeanPropertyAnnot) makeBeans("is")
    }

    def validateParam(tree: ValDef) {
      tree.symbol setAnnotations deriveAnnotations(tree, ParamTargetClass, true)
    }
  }
}