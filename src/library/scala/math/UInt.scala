package scala.math

object UInt {
  
  /** Implicit conversion from `Int` to `UInt`.
   */
  implicit def int2UInt(i: Int): UInt = UInt(i)

}

case class UInt(val signed: Int) extends ScalaNumber with ScalaNumericConversions {
  
  def intValue: Int = signed
  def longValue: Long = signed & 0xffffffffL
  def floatValue: Float = longValue
  def doubleValue: Double = longValue
  protected[scala] def isWhole = true
  def underlying: Object = signed.asInstanceOf[Object]
  
  /** Is this unsigned number equal to its argument?
   */
  override def equals (that: Any): Boolean = that match {
    case num: Number =>  signed == num.intValue
    case _ => false
  }

  /** Is this unsigned number equal to the unsigned argument?
   *  This is a more efficient method than going through `equals`.
   */
  def == (that: UInt): Boolean = this.signed == that.signed

  /** Is this unsigned number different from the unsigned argument?
   *  This is a more efficient method than going through `equals`.
   */
  def != (that: UInt): Boolean = this.signed != that.signed

  /** Is this unsigned number less or equal than its unsigned argument?
   *  @return `this >= that`
   */
  @inline def <= (that: UInt) = {
    val i = this.signed
    val j = that.signed
    if (i >= 0) i <= j || j < 0 else j <= i
  }

  /** Is this unsigned number less than its unsigned argument?
   *  @return `this >= that`
   */
  @inline def < (that: UInt) = {
    val i = this.signed
    val j = that.signed
    if (i >= 0) i < j || j < 0 else j < i
  }
    
  /** Is this unsigned number greater or equal than the unsigned argument?
   *  @return `this >= that`
   */
  def >= (that: UInt) = that <= this
    
  /** Is this unsigned number greater than the unsigned argument?
   *  @return `this > that`
   */
  def > (that: UInt) = that < this
  
  /** This unsigned number plus the unsigned argument
   *  @return `this + that`
   */
  def + (that: UInt) = UInt(this.signed + that.signed)
  
  /** This unsigned number minus the unsigned argument
   *  @return `this - that`
   */
  def - (that: UInt) = UInt(this.signed - that.signed)
  
  /** This unsigned number multiplied by the unsigned argument
   *  @return `this * that`
   */
  def * (that: UInt) = UInt(this.signed * that.signed)

  /** This unsigned number shifted left by the unsigned argument
   *  @return `this << that`
   */
  def << (shift: UInt) = UInt(this.signed << shift.signed)

  /** This unsigned number shifted right by the unsigned argument
   *  @return `this << that`
   */
  def >> (shift: UInt) = UInt(this.signed >> shift.signed)

  /** The bitwise and of this unsigned number with the unsigned argument
   *  @return `this & that`
   */
  def & (that: UInt) = UInt(this.signed & that.signed)

  /** The bitwise or of this unsigned number with the unsigned argument
   *  @return `this & that`
   */
  def | (that: UInt) = UInt(this.signed | that.signed)

  /** The bitwise exclusive or of this unsigned number with the unsigned argument
   *  @return `this ^ that`
   */
  def ^ (that: UInt) = UInt(this.signed ^ that.signed)

  /** The bitwise complement of this unsigned number
   *  @return `~ this`
   */
  def unary_~ (that: UInt) = UInt(this.signed)

  /** This unsigned number dividied by its unsigned argument
   *  @return `this / that`
   */
  def / (that: UInt) = UInt((this.longValue / that.longValue).toInt)

  /** This unsigned number modulo its unsigned argument
   *  @return `this % that`
   */
  def % (that: UInt) = UInt((this.longValue % that.longValue).toInt)
  
  override def toString = this.longValue.toString
}

