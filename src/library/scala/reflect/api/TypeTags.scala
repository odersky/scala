/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

import java.lang.{Class => jClass}

trait TypeTags { self: Universe =>

  case class ClassTag[T](erasure: jClass[_])
  
  /** A tagged type tree */
  case class TypeTag[T](tpe: Type) 

  class GroundTypeTag[T](tpe: Type) extends TypeTag[T](tpe)

  object GroundTypeTag {
    def apply[T](tpe: Type) = new GroundTypeTag[T](tpe)
    def unapply[T](ttag: GroundTypeTag[T]): Option[Type] = Some(ttag.tpe)
  }
  
  // [Martin]: why needed? I.e. Why not use implicitly?
  def typeTag[T](implicit typeTag: TypeTag[T]) = typeTag
}

