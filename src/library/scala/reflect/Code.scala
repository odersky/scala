/*                     __                                               *\
**     ________ ___   / /  ___     Scala API                            **
**    / __/ __// _ | / /  / _ |    (c) 2002-2011, LAMP/EPFL             **
**  __\ \/ /__/ __ |/ /__/ __ |    http://scala-lang.org/               **
** /____/\___/_/ |_/____/_/ | |                                         **
**                          |/                                          **
\*                                                                      */



package scala.reflect

object Code {
  // crashes the compiler. why?!
//  def lift[A: Manifest](tree: A): Code[A] =
//    throw new Error("compiler bug: Code.lift[" + implicitly[Manifest[A]] + "] should have been compiled away")
  def lift[A](tree: A): Code[A] =
    throw new Error("compiler bug: Code.lift should have been compiled away")
  def ulift(tree: Any): Ucode =
    throw new Error("compiler bug: Code.ulift should have been compiled away")
  def typed[A](tree: Ucode): Code[A] =
    throw new Error("compiler bug: Code.typed should have been compiled away")
  def splice[A](tree: Code[A]): A =
    throw new Error("compiler bug: Code.splice should have been compiled away")
  def usplice(tree: Ucode): Magic =
    throw new Error("compiler bug: Code.usplice should have been compiled away")
}

class Code[T: Manifest](tree: scala.reflect.mirror.Tree) extends Ucode(tree) {
  val manifest = implicitly[Manifest[T]]
  override def toString = "Code(tree = "+tree+", manifest = "+manifest+")"
}

object Ucode {
  def lift(tree: Any): Ucode =
    throw new Error("compiler bug: Ucode.lift should have been compiled away")
  def splice[A](tree: Ucode): Magic =
    throw new Error("compiler bug: Ucode.splice should have been compiled away")
}

class Ucode(val tree: scala.reflect.mirror.Tree)

// magic type that gets returned by an untyped splice
// attempting to do anything with something of this type generates itself regardless of the operation
// this is necessary to typecheck untyped splices, which are necessary to build quasiquotations on top of lifters
// IMPORTANT NOTE: THIS IS A PRELIMINARY VERSION, DON'T HATE ME FOR THE NAME!
// in production version, name will be changed into something more suitable 
// and all the marker symbols and types will be dynamically generated rather than declared here
class Magic { def magic(magic: Magic*): Magic = this }
object Magic

trait CodeImplicits {
  implicit def ucode2tree(ucode: Ucode) = ucode.tree
  
  implicit def tree2ucode(tree: Tree) = new Ucode(tree)
}

trait QuasiquoteImplicits {
  implicit def quasiquote2reify(quasiquote: Quasiquote) = 
    new ReifyQuasiquote(quasiquote)
  
  class ReifyQuasiquote(quasiquote: Quasiquote) {
    // todo. to be transformed into a macro
    def c() = ???
  }
}