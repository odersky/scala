import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Macros {
  def foo(xs: Int*) = macro Impls.foo
}

object Test extends App {
  import scala.reflect.mirror._
  val tree = Apply(Select(Ident("Macros"), newTermName("foo")), List(Typed(Apply(Ident(definitions.ListModule), List(Literal(Constant(1)), Literal(Constant(2)))), Ident(tpnme.WILDCARD_STAR))))

  val stderr = new java.io.ByteArrayOutputStream()
  Console.setErr(new java.io.PrintStream(stderr))

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  try { println(toolbox.runExpr(tree)) }
  catch { case ex: Throwable => println(stderr); println(ex) }
}