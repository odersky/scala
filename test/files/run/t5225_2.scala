import scala.reflect._
import scala.reflect.api._

object Test extends App {
  val tree = scala.reflect.mirror.reify{def foo(@cloneable x: Int) = ""}.tree
  println(tree.toString)
}