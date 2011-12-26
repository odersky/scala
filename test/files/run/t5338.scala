import scala.reflect.Code

object Test extends App {
  val x: Code[Int] = 2 * 5
  val y = Code.lift{2 * 5}
  val z = Code.lift[Int]{2 * 5}
  val w: Code[Int] = Code.lift{2 * 5}
  def v(a: Code[Int]): Code[Int] = a
  println(x.tree)
  println(y.tree)
  println(z.tree)
  println(w.tree)
  println(v(2 * 5).tree)

  val u = Code.lift[Unit]{def x = 2 * 5;}
  println(u.tree)

  def t[T](ys: List[T]) = {
    val s: Code[Int => Int] = x => x + ys.length + 2 * 5
    s
  }
  println(t(List(1, 2, 3)).tree)
}