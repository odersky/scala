import scala.reflect.Code._

object Test extends App {
  def tt = lift{lift{2}}
  def tu = lift{ulift{2}}
  def ut = ulift{lift{2}}
  def uu = ulift{ulift{2}}
  def two = lift{2}
  def four = lift{splice(two) + splice(two)}
  val eight1 = splice(four) + splice(four)
  val eight2 = usplice(four) + usplice(four)
  println(eight1 + eight2)
}
