package scala.runtime

class StringContextQuasiquote(quasiquote: reflect.Quasiquote) {
  // todo. to be transformed into a macro
  def s() = ???
}

class StringContext(first: String, rest: String*) {
  def show(exprs: Any*) = {
    val b = new java.lang.StringBuffer(first)
    val it = rest.iterator
    for (e <- exprs) b append e.show append it.next
    b.toString
  }
}
