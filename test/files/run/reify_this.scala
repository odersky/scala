import scala.reflect.mirror._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

trait Eval {
  def eval(code: Expr[_]): Any = eval(code.tree)

  def eval(tree: Tree): Any = {
    val settings = new Settings
    val reporter = new ConsoleReporter(settings)
    val toolbox = new ToolBox(reporter)
    toolbox.runExpr(tree)
  }
}

object Test extends App with Eval {
  // select a value from package
  eval(reify{println("foo")})
  eval(reify{println((new Object).toString == (new Object).toString)})

  // select a type from package
  eval(reify{val x: Any = 2; println(x)})
  eval(reify{val x: Object = "bar"; println(x)})

  // select a value from module
  val x = 2
  eval(reify{println(x)})
}
