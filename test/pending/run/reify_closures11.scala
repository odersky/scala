import scala.reflect.mirror._
import scala.tools.nsc.reporters._
import scala.tools.nsc.Settings
import reflect.runtime.Mirror.ToolBox

object Test extends App {
  def fun() = {
    def z() = 2
    reify{z}
  }

  val reporter = new ConsoleReporter(new Settings)
  val toolbox = new ToolBox(reporter)
  val dyn = toolbox.runExpr(fun().tree)
  val foo = dyn.asInstanceOf[Int]
  println(foo)
}
