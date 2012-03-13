package scala.reflect.makro
package runtime

trait Utils {
  self: Context =>

  import mirror._

  def literalNull = Expr[Null](Literal(Constant(null)))
  def literalUnit = Expr[Unit](Literal(Constant(())))
  def literalTrue = Expr[Boolean](Literal(Constant(true)))
  def literalFalse = Expr[Boolean](Literal(Constant(false)))
  def literal(x: Boolean) = Expr[Boolean](Literal(Constant(x)))
  def literal(x: Byte) = Expr[Byte](Literal(Constant(x)))
  def literal(x: Short) = Expr[Short](Literal(Constant(x)))
  def literal(x: Int) = Expr[Int](Literal(Constant(x)))
  def literal(x: Long) = Expr[Long](Literal(Constant(x)))
  def literal(x: Float) = Expr[Float](Literal(Constant(x)))
  def literal(x: Double) = Expr[Double](Literal(Constant(x)))
  def literal(x: String) = Expr[String](Literal(Constant(x)))
  def literal(x: Char) = Expr[Char](Literal(Constant(x)))
}
