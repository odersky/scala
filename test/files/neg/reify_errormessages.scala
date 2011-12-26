import scala.reflect.Code
import scala.reflect.Code._

object Test {
  def e0 = lift{splice{thereisnosuchsymbol}}
  def e1 = lift{2 * "foobar"}
  def e2 = lift{2 * splice{lift{"foobar"}}}
  def e3 = lift{2 * usplice{lift{"foobar"}}}
  def e4 = ulift{2 * "foobar"}
  def e5 = ulift{2 * splice{lift{"foobar"}}}
  def e6 = ulift{2 * usplice{lift{"foobar"}}}
  def e7 = ({x: Code[Int]} => x)("x")
  def e8: Code[Int] = "x"
}