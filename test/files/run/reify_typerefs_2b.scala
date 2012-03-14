import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

package foo {
  object Expression {
    override def toString = "Expression"
  }
}

object Test extends App {
  val code = scala.reflect.mirror.reify{
    List(foo.Expression, foo.Expression)
  };

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val evaluated = toolbox.runExpr(code.tree)
  println("evaluated = " + evaluated)
}
