import scala.reflect.Code._

object Test extends App {
  def two = ulift{2}
  def four = ulift{splice(two) + usplice(two)}
}
