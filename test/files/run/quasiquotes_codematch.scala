import scala.reflect.mirror._

object Test extends App {
  val x = c"2"
  val y = c"2"
  val code = c"$x + $y"
  code.tree match {
    case c"$two + 2" => println("success: " + showRaw(two))
    case _ => println("failure")
  }
}