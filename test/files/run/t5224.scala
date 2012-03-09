import scala.reflect._
import scala.reflect.api._

class Foo(bar: String) extends ClassfileAnnotation

object Test extends App {
  val tree = scala.reflect.mirror.reify{@Foo(bar = "qwe") class C}.tree
  println(tree.toString)
}