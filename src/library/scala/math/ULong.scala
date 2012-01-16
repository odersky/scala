package scala.math

object ULong extends App {

  def apply(signed: Long) = new ULong(signed)
  
  /** Implicit conversion from `Int` to `ULong`.
   */
  implicit def int2ULong(i: Int): ULong = ULong(i)

  /** Implicit conversion from `Long` to `ULong`.
   */
  implicit def long2ULong(i: Long): ULong = ULong(i)

  /** Implicit conversio from `ULong` to `Long`.
   */
  implicit def ulong2Long(u: ULong): Long = u.signed

  /* ------ Algorithm for unsigned division and modulo
   * 
   *  special case d = 0, d = 1, d < 0, so assume d > 1 in the following.
   *
   *  use laws (for unsigned A, B, C):
   *
   *    (A + B) / C == A / C + B / C + (A % C + B % C) / C
   *    (A + B) % C == (A % C + B % C) % C
   *
   *  and decompose 
   *
   *    n = (nh << 1) + nl
   *
   *  where
   *
   *    nh = n >>> 1
   *    nl = n & 1
   *
   *  to derive:
   *
   *  nh % d + nh % d 
   *  = nh % d << 1           (pick this one if nh % d is still unsiged)
   *  = (nh % d << 1) - d + d (pick this one if not)
   *
   *  let (dmod, correction) =
   *    (nh % d << 1, 0)        if nh % d << 1  <=  Long.MaxValue
   *    (nh % d << 1 - d, 1)    otherwise
   *
   *  (nh << 1) / d 
   *  = (nh + nh) / d
   *  = (nh / d + nh / d) + (nh % d + nh % d) / d
   *  = (nh / d << 1) + dmod / d + correction
   *
   *  (nh << 1) % d
   *  = (nh % d + nh % d) % d
   *  = dmod % d
   *
   *  nl / d = 0
   *  nl % d = nl    (since d != 0, d != 1)
   *
   *  n / d 
   *  = ((nh << 1) + nl) / d
   *  = (nh << 1) / d + ((nh << 1) % d + nl) / d
   *  = (nh / d << 1) + dmod / d + correction + (dmod % d + nl) / d
   *
   *  n % d
   *  = (nh << 1 + nl) % d
   *  = ((nh << 1) % d + nl) % d
   *  = (dmod % d + nl) % d
   *
   */

  /** @return (n / d, n % d) where / and % are unsigned division and modulo
   */
  def udivmod (n: Long, d: Long): (Long, Long) = {
    if (d > 0) {
      if (d == 1) (n, 0)
      else {
        val nh = n >>> 1
        val nl = n & 1
        var dmod = nh % d << 1
        var correction = 0
        if (dmod < 0) { dmod -= d; correction = 1; assert(dmod >= 0) }
        val y = dmod % d + nl
        val rn = (nh / d << 1) + dmod / d + correction + y / d
        val rd = y % d
        (rn, rd)
      }
    } else if (n >= 0 || n < d) { 
      (0, n)
    } else {
      (1, n - d)
    }
  }
}

class ULong(val signed: Long) extends ScalaNumber with ScalaNumericConversions {

  def intValue: Int = signed.toInt
  def longValue: Long = signed
  def floatValue: Float = signed
  def doubleValue: Double = signed
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
  def == (that: ULong): Boolean = this.signed == that.signed

  /** Is this unsigned number different from the unsigned argument?
   *  This is a more efficient method than going through `equals`.
   */
  def != (that: ULong): Boolean = this.signed != that.signed

  /** Is this unsigned number less or equal than its unsigned argument?
   *  @return `this >= that`
   */
  @inline def <= (that: ULong) = {
    val i = this.signed
    val j = that.signed
    if (i >= 0) i <= j || j < 0 else j <= i
  }

  /** Is this unsigned number less than its unsigned argument?
   *  @return `this >= that`
   */
  @inline def < (that: ULong) = {
    val i = this.signed
    val j = that.signed
    if (i >= 0) i < j || j < 0 else j < i
  }
    
  /** Is this unsigned number greater or equal than the unsigned argument?
   *  @return `this >= that`
   */
  def >= (that: ULong) = that <= this
    
  /** Is this unsigned number greater than the unsigned argument?
   *  @return `this > that`
   */
  def > (that: ULong) = that < this
  
  /** This unsigned number plus the unsigned argument
   *  @return `this + that`
   */
  def + (that: ULong) = ULong(this.signed + that.signed)
  
  /** This unsigned number minus the unsigned argument
   *  @return `this - that`
   */
  def - (that: ULong) = ULong(this.signed - that.signed)
  
  /** This unsigned number multiplied by the unsigned argument
   *  @return `this * that`
   */
  def * (that: ULong) = ULong(this.signed * that.signed)

  /** This unsigned number shifted left by the unsigned argument
   *  @return `this << that`
   */
  def << (shift: ULong) = ULong(this.signed << shift.signed)

  /** This unsigned number shifted right by the unsigned argument
   *  @return `this << that`
   */
  def >> (shift: ULong) = ULong(this.signed >> shift.signed)

  /** The bitwise and of this unsigned number with the unsigned argument
   *  @return `this & that`
   */
  def & (that: ULong) = ULong(this.signed & that.signed)

  /** The bitwise or of this unsigned number with the unsigned argument
   *  @return `this & that`
   */
  def | (that: ULong) = ULong(this.signed | that.signed)

  /** The bitwise exclusive or of this unsigned number with the unsigned argument
   *  @return `this ^ that`
   */
  def ^ (that: ULong) = ULong(this.signed ^ that.signed)

  /** The bitwise complement of this unsigned number
   *  @return `~ this`
   */
  def unary_~ (that: ULong) = ULong(this.signed)

  /** This unsigned number dividied by its unsigned argument
   *  @return `this / that`
   */
  def / (that: ULong): ULong = { // udivmod(this.signed, that.signed)._1
    val n = this.signed
    val d = that.signed
    ULong {
      if (d > 0) {
        if (d == 1) n
        else {
          val nh = n >>> 1
          val nl = n & 1
          var dmod = nh % d << 1
          var correction = 0
          if (dmod < 0) { dmod -= d; correction = 1 }
          val y = dmod % d + nl
          (nh / d << 1) + dmod / d + correction + y / d
        }
      } else if (n >= 0 || n < d) { 
        0
      } else {
        1
      }
    }
  }

  /** This unsigned number modulo its unsigned argument
   *  @return `this % that`
   */
  def % (that: ULong) = { // udivmod(this.signed, that.signed)._2
    val n = this.signed
    val d = that.signed
    ULong {
      if (d > 0) {
        if (d == 1) 0
        else {
          val nh = n >>> 1
          val nl = n & 1
          var dmod = nh % d << 1
          if (dmod < 0) { dmod -= d }
          val y = dmod % d + nl
          y % d
        }
      } else if (n >= 0 || n < d) { 
        n
      } else {
        n - d
      }
    }
  }

  override def toString: String = 
    if (signed > 0) signed.toString
    else {
      val (hi, lo) = ULong.udivmod(this, 10)
      hi.toString + lo.toString
    }
}


