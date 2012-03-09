/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

trait TypeTags { self: Universe =>

  /** A tagged type tree */
  case class TypeTag[T](tpe: Type) {
    // assert(tpe =:= T.typeTag.tpe)
    // would like to check the above assertion, but Manifests need to be part of the universe first. See ``Expr''.
    override def toString = "TypeTag["+tpe+"]"
  }

  def typeTag[T](implicit typeTag: TypeTag[T]) = typeTag
}

