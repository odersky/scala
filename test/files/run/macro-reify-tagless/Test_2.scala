import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  //val list: List[String] = Macros.foo("hello world")
  //println(list)

  import scala.reflect.mirror._
  val tpt = AppliedTypeTree(Ident(definitions.ListClass), List(Ident(definitions.StringClass)))
  val rhs = Apply(Select(Ident("Macros"), newTermName("foo")), List(Literal(Constant("hello world"))))
  val list = ValDef(NoMods, newTermName("list"), tpt, rhs)
  val tree = Block(list, Apply(Select(Ident(definitions.PredefModule), newTermName("println")), List(Ident(list.name))))

  val stderr = new java.io.ByteArrayOutputStream()
  Console.setErr(new java.io.PrintStream(stderr))

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  try { println(toolbox.runExpr(tree)) }
  catch { case ex: Throwable => println(stderr); println(ex) }
}
