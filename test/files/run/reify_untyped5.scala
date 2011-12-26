import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def utwo = ulift{2}
  def two = typed[Int]{utwo}
  def four = lift{splice(typed[Int](utwo)) + splice(two)}

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(four)
  println(toolbox.runExpr(ttree))
}
