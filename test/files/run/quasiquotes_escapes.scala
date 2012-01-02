object Test extends App {
  val foo = 2
  val bar = 3
  val bar3 = 33
  val bar$bar = "bar$bar"
  val bar3$bar = "bar3$bar"
  println(s"foo\nbar")
  println(s"\u0068i")
  println(s"hi x$foo = \150\151\150\151\u0021")
  println(s"si\u0078 is the same as ${foo*bar}")
  println(s"$bar\u0033")
  println(s"${bar\u0033}$bar")
  println(s"$bar\u0033$bar")
  println(s"\u0033$bar$bar")
}