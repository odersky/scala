package scala.reflect.makro
package runtime

import scala.reflect.internal.Flags._

trait Symbols {
  self: Context =>

  import mirror._

  def captureVariable(vble: Symbol): Unit = vble setFlag CAPTURED

  def referenceCapturedVariable(vble: Symbol): Tree = ReferenceToBoxed(Ident(vble))
}