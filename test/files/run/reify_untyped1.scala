import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def two = ulift{2}
  def four = ulift{usplice(two) + usplice(two)}

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(four)
  println(toolbox.runExpr(ttree))
}
