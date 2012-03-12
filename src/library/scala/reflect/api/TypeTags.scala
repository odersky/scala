/* NSC -- new Scala compiler
 * Copyright 2005-2011 LAMP/EPFL
 * @author  Martin Odersky
 */

package scala.reflect
package api

import java.lang.{Class => jClass}

trait TypeTags { self: Universe =>

  abstract class AbstractTag[T] {
    def tpe: Type
    def erasure: jClass[_]
  }

  abstract class ClassTag[T] extends AbstractTag[T] {
    def tpe = classToType(erasure)
  }
  
  def ClassTag[T](erasure: jClass[_]) = new ClassTag[T] {
    def erasure = _erasure
  }

  /** A tagged type tree */
  abstract class TypeTag[T] extends AbstractTag[T] {
    def erasure = typeToClass(tpe)
  }

  def TypeTag[T](_tpe: Type) = new TypeTag[T] {
    def tpe = _tpe
  }

  class GroundTypeTag[T] extends TypeTag[T]

  def GroundTypeTag(_tpe: Type) = new GroundTypeTag[T] {
    def tpe = _tpe
  }
  
  // [Martin]: why needed? I.e. Why not use implicitly?
  def typeTag[T](implicit typeTag: TypeTag[T]) = typeTag

    /** Maps a Java class to a Scala type reference
   *  @param   clazz    The Java class object
   *  @return  A type (of kind `TypeRef`, or `ExistentialType` if `clazz` is polymorphic)
   *           that represents the class with all type parameters unknown
   *           (i.e. any type parameters of `clazz` are existentially quantified).
   *  */
  def classToType(clazz: java.lang.Class[_]): Type

    /** Maps a Scala type to the corresponding Java class object
   */
  def typeToClass(tpe: Type): java.lang.Class[_]
}

