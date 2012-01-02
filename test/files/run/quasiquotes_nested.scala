object Test extends App {
  val foo = 2
  val bar = 3
  println(s"foo${s""}")
  println(s"foo${s"bar"}")
  println(s"foo${s"$bar"}")
  println(s"foo${s"bar$bar"}")
  println(s"foo${s"$bar?bar!"}")
  println(s"foo${s"${bar}"}")
  println(s"foo${s"bar${bar}"}")
  println(s"foo${s"${bar}?bar!"}")
  println(s"foo${s"${bar}?bar${s"${bar}!!"}!"}")
}