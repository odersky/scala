package scala.math

object UInt {
  
  /** Implicit conversion from `Int` to `UInt`.
   */
  @inline implicit def int2UInt(i: Int): UInt = new UInt(i)

}

/** An experimental class for 

class UInt(val signed: Int) extends AnyVal {
  
  /** Returns the value of this as a [[scala.Char]]. This may involve
    * rounding or truncation.
    */
  def toChar: Char = signed.toChar

  /** Returns the value of this as a [[scala.Byte]]. This may involve
    * rounding or truncation.
    */
  def toByte: Byte = signed.toByte

  /** Returns the value of this as a [[scala.Short]]. This may involve
    * rounding or truncation.
    */
  def toShort: Short = signed.toShort

  /** Returns the value of this as an [[scala.Int]]. This may involve
    * rounding or truncation.
    */
  def toInt: Int = signed

  /** Returns the value of this as a [[scala.Long]]. This may involve
    * rounding or truncation.
    */
  def toLong: Long = signed & 0xffffffffL

  /** Returns the value of this as a [[scala.Float]]. This may involve
    * rounding or truncation.
    */
  def toFloat: Float = toLong.toFloat

  /** Returns the value of this as a [[scala.Double]]. This may involve
    * rounding or truncation.
    */
  def toDouble: Double = toLong.toFloat

  /** Returns `true` iff this is a whole number
   */
  def isWhole = true

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Byte]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidByte  = toInt == toByte

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Short]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidShort = toInt == toShort

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Int]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidInt   = toLong == toInt

  /** Returns `true` iff this has a zero fractional part, and is within the
    * range of [[scala.Char]] MinValue and MaxValue; otherwise returns `false`.
    */
  def isValidChar  = toInt == toChar
  
  /** Is this unsigned number equal to its argument?
   */
  override def equals (that: Any): Boolean = that match {
    case num: Number =>  signed == num.intValue
    case _ => false
  }

  /** Is this unsigned number equal to the unsigned argument?
   *  This is a more efficient method than going through `equals`.
   */
  @inline def == (that: UInt): Boolean = this.signed == that.signed

  /** Is this unsigned number different from the unsigned argument?
   *  This is a more efficient method than going through `equals`.
   */
  @inline def != (that: UInt): Boolean = this.signed != that.signed

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
  @inline def >= (that: UInt) = that <= this
    
  /** Is this unsigned number greater than the unsigned argument?
   *  @return `this > that`
   */
  @inline def > (that: UInt) = that < this
  
  /** This unsigned number plus the unsigned argument
   *  @return `this + that`
   */
  @inline def + (that: UInt) = UInt(this.signed + that.signed)
  
  /** This unsigned number minus the unsigned argument
   *  @return `this - that`
   */
  @inline def - (that: UInt) = UInt(this.signed - that.signed)
  
  /** This unsigned number multiplied by the unsigned argument
   *  @return `this * that`
   */
  @inline def * (that: UInt) = UInt(this.signed * that.signed)

  /** This unsigned number shifted left by the unsigned argument
   *  @return `this << that`
   */
  @inline def << (shift: UInt) = UInt(this.signed << shift.signed)

  /** This unsigned number shifted right by the unsigned argument
   *  @return `this << that`
   */
  @inline def >> (shift: UInt) = UInt(this.signed >> shift.signed)

  /** The bitwise and of this unsigned number with the unsigned argument
   *  @return `this & that`
   */
  @inline def & (that: UInt) = UInt(this.signed & that.signed)

  /** The bitwise or of this unsigned number with the unsigned argument
   *  @return `this & that`
   */
  @inline def | (that: UInt) = UInt(this.signed | that.signed)

  /** The bitwise exclusive or of this unsigned number with the unsigned argument
   *  @return `this ^ that`
   */
  @inline def ^ (that: UInt) = UInt(this.signed ^ that.signed)

  /** The bitwise complement of this unsigned number
   *  @return `~ this`
   */
  @inline def unary_~ (that: UInt) = UInt(this.signed)

  /** This unsigned number dividied by its unsigned argument
   *  @return `this / that`
   */
  @inline def / (that: UInt) = UInt((this.longValue / that.longValue).toInt)

  /** This unsigned number modulo its unsigned argument
   *  @return `this % that`
   */
  @inline def % (that: UInt) = UInt((this.longValue % that.longValue).toInt)
  
  override def toString = this.longValue.toString
}

