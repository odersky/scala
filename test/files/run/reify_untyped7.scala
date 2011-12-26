import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def two = ulift{2}
  def four1 = ulift{usplice(two) + usplice(two)}
  def four2 = ulift{2 + usplice(two)}
  def four3 = ulift{usplice(two) + 2}
  def four4 = ulift{2 + 2}
  def four5 = ulift{4.toString}
  def four6 = ulift{usplice{four5}.toString}

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  var ttree: scala.reflect.mirror.Tree = null;

  ttree = toolbox.typeCheck(four1)
  println(toolbox.runExpr(ttree))
  ttree = toolbox.typeCheck(four2)
  println(toolbox.runExpr(ttree))
  ttree = toolbox.typeCheck(four3)
  println(toolbox.runExpr(ttree))
  ttree = toolbox.typeCheck(four4)
  println(toolbox.runExpr(ttree))
  ttree = toolbox.typeCheck(four5)
  println(toolbox.runExpr(ttree))
  ttree = toolbox.typeCheck(four6)
  println(toolbox.runExpr(ttree))
}
