package scala.reflect.makro
package runtime

trait Symbols {
  self: Context =>

  def captureVariable(vble: Symbol): Unit = mirror.captureVariable(vble)

  def referenceCapturedVariable(vble: Symbol): Tree = mirror.referenceCapturedVariable(vble)
}