package scala.tools.nsc
package typechecker

import scala.collection.{ mutable, immutable }
import scala.tools.nsc.util.BatchSourceFile

trait Quasiquoter {
  self: Analyzer =>

  import global._
  import definitions._

  object QuasiquoteLiteral {
    def unapply(tree: Tree): Option[QuasiquoteLiteral] = tree match {
      case Apply(Select(Apply(Select(New(tpt), ctorName), List(ttparts, tsplices, ttformats)), providerName), List())
      if ctorName == nme.CONSTRUCTOR && tpt.tpe <:< QuasiquoteLiteralClass.tpe =>
        val provider = providerName.toString
        provider match {
          case "s" => Some(new StringInterpolationLiteral(tree) { override val title = "string interpolation" })
          case "c" => Some(new CodeQuotationLiteral(tree) { override val title = "code quotation" })
          case _ => throw new TypeError("unknown quasiquote provider: " + provider)
        }
      case _ =>
        None
    }
  }

  object QuasiquotePattern {
    def unapply(tree: Tree): Option[QuasiquotePattern] = tree match {
      case CaseDef(Apply(Select(Apply(Select(New(tpt), ctorName), List(ttparts, tsplices, ttformats)), providerName), List()), _, _)
      if ctorName == nme.CONSTRUCTOR && tpt.tpe <:< QuasiquotePatternClass.tpe =>
        val provider = providerName.toString
        provider match {
          case "s" => throw new TypeError("pattern matching against string interpolation is not supported")
          case "c" => Some(new CodeQuotationPattern(tree) { override val title = "code pattern" })
          case _ => throw new TypeError("unknown quasiquote provider: " + provider)
        }
      case _ =>
        None
    }
  }

  abstract class Quasiquote(tree: Tree) {
    val title: String
    override def toString = tree.toString
  }

  abstract class QuasiquoteLiteral(tree: Tree) extends Quasiquote(tree: Tree) {
    val Apply(Select(Apply(Select(New(tpt), ctorName), List(ttparts, tsplices, ttformats)), providerName), List()) = tree
    assert(ctorName == nme.CONSTRUCTOR)
    assert(tpt.tpe <:< QuasiquoteLiteralClass.tpe)

    val Apply(Ident(_), tparts) = ttparts
    val parts = tparts map { case Literal(Constant(part: String)) => part }
    val Apply(Ident(_), splices) = tsplices
    val Apply(Ident(_), tformats) = ttformats
    val formats = tformats map { case Literal(Constant(format: String)) => format }
    val provider = providerName.toString
    def expand(context: Context): Tree

    override val title: String = "quasiquote literal"
  }

  abstract class QuasiquotePattern(tree: Tree) extends Quasiquote(tree: Tree) {
    val CaseDef(casedef @ Apply(Select(Apply(Select(New(tpt), ctorName), List(ttparts, tsplices, ttformats)), providerName), List()), guard0, body0) = tree
    assert(ctorName == nme.CONSTRUCTOR)
    assert(tpt.tpe <:< QuasiquotePatternClass.tpe)
    assert(guard0 == EmptyTree)

    val Apply(Ident(_), tparts) = ttparts
    val parts = tparts map { case Literal(Constant(part: String)) => part }
    val Apply(Ident(_), splices) = tsplices
    val Apply(Ident(_), tformats) = ttformats
    val formats = tformats map { case Literal(Constant(format: String)) => format }
    val provider = providerName.toString

    val oldguard = guard0
    val oldbody = body0
    def guard = guard0
    def guard_=(tree: Tree): Unit = ???
    def body = body0
    def body_=(tree: Tree): Unit = ???
    def expandpat(context: Context): Tree
    def expand(context: Context): CaseDef = {
      val expandedpat = expandpat(context)
      val expandedguard = guard
      val expandedbody = body
      atPos(tree.pos)(CaseDef(expandedpat, expandedguard, expandedbody))
    }

    override val title: String = "quasiquote pattern"
  }

  class StringInterpolationLiteral(tree: Tree) extends QuasiquoteLiteral(tree) {
    override def expand(context: Context) = {
      val ctor = Select(New(TypeTree(StringContextClass.tpe)), nme.CONSTRUCTOR)

      val stringcontext = Apply(ctor, tparts map { tpart =>
        val Literal(Constant(part: String)) = tpart
        val file = new BatchSourceFile(context.unit.source.file, "\"" + part + "\"") {
          override def positionInUltimateSource(pos: Position) = {
            pos.withSource(context.unit.source, tpart.pos.point)
          }
        }
        val unit = new CompilationUnit(file)
        val parser = new syntaxAnalyzer.UnitParser(unit)
        if (parser.in.errOffset != -1) throw new TypeError("error in string interpolation")
        Literal(Constant(parser.in.strVal))
      })

      val show = Apply(Select(stringcontext, StringContext_show), (for ((splice, format) <- splices zip formats) yield {
        if (format != "") {
          val ctor = Select(New(TypeTree(FormattedClass.tpe)), nme.CONSTRUCTOR)
          val formatted = Apply(ctor, List(splice, Literal(Constant("%" + format))))
          Select(formatted, Formatted_show)
        } else {
          splice
        }
      }))

      show
    }
  }

  // string manipulation is, obviously, a hack that is here only because it's a prototype
  class CodeQuotationLiteral(tree: Tree) extends QuasiquoteLiteral(tree) {
    override def expand(context: Context) = {
      var resultIsTyped = true
      def lift(code: String) = "scala.reflect.Code.lift{" + code + "}"
      def ulift(code: String) = "scala.reflect.Code.ulift{" + code + "}"
      def splice(code: String) = "scala.reflect.Code.splice{" + code + "}"
      def usplice(code: String) = "scala.reflect.Code.usplice{" + code + "}"
      def typed(code: String, tpe: String) = "scala.reflect.Code.typed[" + tpe + "]{" + code + "}"

      val (ssplices, istypeds) = (splices zip formats) map { case (untypedsplice, format) =>
        val context1 = context.make(untypedsplice)
        val typer1 = newTyper(context1)
        val typedsplice = try { typer1.typed(untypedsplice) } catch { case ex: TypeError => null }
        val istyped = (typedsplice != null && typedsplice.tpe <:< CodeClass.tpe) || format != null

        var code = typedsplice.toString
        if (format != "") code = typed(code, format)
        code = if (istyped) splice(code) else usplice(code)
        (code, istyped)
      } unzip

      val b = new java.lang.StringBuffer(parts.head)
      (ssplices zip parts.tail) foreach { case (ssplice, part) => b append ssplice append part }
      val istyped = istypeds forall identity
      val code = if (istyped) lift(b.toString) else ulift(b.toString)

      val file = new BatchSourceFile(context.unit.source.file, code)
      val unit = new CompilationUnit(file)
      val parser = new syntaxAnalyzer.UnitParser(unit)
      val quotation = parser.expr()
      if (reporter.hasErrors) throw new TypeError("error in code quotation")
      quotation
    }
  }

  // string manipulation is, obviously, a hack that is here only because it's a prototype
  class CodeQuotationPattern(tree: Tree) extends QuasiquotePattern(tree) {
    override var guard: Tree = if (oldguard != EmptyTree) oldguard else Literal(Constant(true))

    def mirror: Tree = Select(Select(Ident("scala"), newTermName("reflect")), newTermName("mirror"))

    def reify(name: Name): Tree = {
      val bindid = newTermName("$name$" + name.toString)

      val lhs = Select(Ident(bindid), newTermName("toString"))
      val rhs = Literal(Constant(name.toString))
      val clause = Apply(Select(lhs, newTermName("$eq$eq")), List(rhs))
      guard = Apply(Select(guard, newTermName("$amp$amp")), List(clause))

      val result = Bind(bindid, Ident(newTermName("_")))
      result
    }

    def reify(const: Constant): Tree = {
      val ctor = Select(mirror, newTermName("Constant"))
      val result = Apply(ctor, List(Literal(Constant(const.value))))
      result
    }

    def reify(pats: List[Tree]): List[Tree] = pats map reify

    def reify(pat: Tree): Tree = pat match {
      case Apply(fun, args) =>
        val ctor = Select(mirror, newTermName("Apply"))
        val result = Apply(ctor, List(reify(fun), Apply(Ident(ListModule), reify(args))))
        result
      case Select(qual, name) =>
        val ctor = Select(mirror, newTermName("Select"))
        val result = Apply(ctor, List(reify(qual), reify(name)))
        result
      case Ident(name) if name startsWith "$bind$" =>
        val bindid = newTermName(name.toString substring 6)
        val result = Bind(bindid, Ident(newTermName("_")))
        result
      case Ident(name) =>
        val ctor = Select(mirror, newTermName("Ident"))
        val result = Apply(ctor, List(reify(name)))
        result
      case Literal(constant) =>
        val ctor = Select(mirror, newTermName("Literal"))
        val result = Apply(ctor, List(reify(constant)))
        result
      case _ =>
        throw new TypeError("unsupported pattern: " + pat)
    }

    override def expandpat(context: Context): Tree = {
      val ssplices = (splices zip formats) map { case (splice, format) =>
        val Ident(identName) = splice
        val ident = splice.toString
        assert(format == "")
        "$bind$" + ident
      }

      val b = new java.lang.StringBuffer(parts.head)
      (ssplices zip parts.tail) foreach { case (ssplice, part) => b append ssplice append part }
      val code = b.toString

      val file = new BatchSourceFile(context.unit.source.file, code)
      val unit = new CompilationUnit(file)
      val parser = new syntaxAnalyzer.UnitParser(unit)
      val pattern = parser.expr()
      if (reporter.hasErrors) throw new TypeError("error in code pattern")

      val reified = reify(pattern)
      reified
    }
  }
}