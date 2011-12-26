import scala.reflect.Code._

object Test extends App {
  def two = lift{2}
  def four = lift{splice(two) + splice(two)}
  val eight = splice(four) + splice(four)
  println(eight)
}
