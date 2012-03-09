import scala.reflect._
import scala.reflect.api._

class Foo extends StaticAnnotation

object Test extends App {
  val tree = scala.reflect.mirror.reify{5: @Foo}.tree
  println(tree.toString)
}