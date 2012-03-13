import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  import scala.reflect.mirror._
  val q = New(AppliedTypeTree(Select(Select(Select(Ident("scala"), newTermName("collection")), newTermName("slick")), newTypeName("Queryable")), List(Ident("Int"))))
  val x = ValDef(NoMods, newTermName("x"), Ident("Int"), EmptyTree)
  val fn = Function(List(x), Apply(Select(Ident(newTermName("x")), newTermName("$plus")), List(Literal(Constant("5")))))
  val tree = Apply(Select(q, newTermName("map")), List(fn))

  val stderr = new java.io.ByteArrayOutputStream()
  Console.setErr(new java.io.PrintStream(stderr))

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  try { println(toolbox.runExpr(tree)) }
  catch { case ex: Throwable => println(stderr); println(ex) }
}