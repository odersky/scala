package scala.reflect
package api

trait Positions {
  self: Universe =>

  type Position
  val NoPosition: Position

  def atPos[T <: Tree](pos: Position)(tree: T): T
}