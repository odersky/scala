object Test extends App {
  val foo = 2
  val bar = 3

  println(s"")
  println(s"foo")
  println(s"$foo")
  println(s"qwe$foo")
  println("==============")
  println("")

  println(s"qwe$foo,oof")
  println(s"$foo,oof")
  println(s"qwe$foo}")
  println(s"qwe${foo}")
  println("==============")
  println("")

  println(s"qwe${foo},oof")
  println(s"${foo},oof")
  println(s"qwe${foo*bar}")
  println(s"${foo*bar}ewq")
  println("==============")
  println("")

  println(s"qwe${foo*bar}rab")
  println(s"qwe${foo*bar:d}")
  println(s"${foo*bar:d}ewq")
  println(s"qwe${foo*bar:d}ddd")
  println("==============")
  println("")

  println(s"qwe${foo*bar: d}")
  println(s"qwe${foo*bar:d }")
  println(s"qwe${foo*bar: d }")
  println(s"qwe${foo*bar :d}")
  println("==============")
  println("")

  println(s"qwe${foo*bar : d}")
  println(s"qwe${foo*bar :d }")
  println(s"qwe${foo*bar : d }")
  println(s"qwe${foo*bar: d }")
  println("==============")
}