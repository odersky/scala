import scala.reflect.Code
import scala.reflect.Ucode
import scala.reflect.Code._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def two = lift{2}
  def ttwo: Code[Int] = two
  def utwo = ulift{2}
  def tutwo = typed[Int]{utwo}
  def uutwo: Ucode = two
  def ten = ulift{usplice(two) + usplice(ttwo) + usplice(utwo) + usplice(tutwo) + usplice(uutwo)}

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val ttree = toolbox.typeCheck(ten)
  println(toolbox.runExpr(ttree))
}
