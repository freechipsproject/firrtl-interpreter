// See LICENSE for license details.
package firrtl_interpreter

import firrtl.ir._

// scalastyle:off number.of.methods
trait Concrete {
  val value : BigInt
  val width : Int
  val lowBitOffset = 0

  def +(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteUInt(v1 + v2, w1.max(w2) + 1)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 + v2, w1.max(w2) + 1)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 + v2, w1.max(w2) + 1)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 + v2, w1.max(w2) + 1)
    }
  }
  def -(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 - v2, w1.max(w2) + 1)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) =>
        val newWidth = (w1 + 2).max(w2 + 1)
        ConcreteSInt(v1 - v2, newWidth)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) =>
        val newWidth = if(w1 == 1) w2 + 1 else (w1 + 1).max(w2 + 2)
        ConcreteSInt(v1 - v2, newWidth)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 - v2, w1.max(w2) + 1)
    }
  }
  def *(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteUInt(v1 * v2, w1 + w2)
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 * v2, w1 + w2)
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) => ConcreteSInt(v1 * v2, w1 + w2)
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) => ConcreteSInt(v1 * v2, w1 + w2)
    }
  }
  def /(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedUInt(w1) }
        else { ConcreteUInt(v1 / v2, w1) }
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedSInt(w1) }
        else { ConcreteSInt(v1 / v2, w1 + 1) }
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedSInt(w1) }
        else { ConcreteSInt(v1 / v2, w1) }
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedSInt(w1) }
        else { ConcreteSInt(v1 / v2, w1 + 1) }
    }
  }
  def %(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1), ConcreteUInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedUInt(w1.min(w2)) }
        else { ConcreteUInt(v1 % v2, w1.min(w2)) }
      case (ConcreteUInt(v1, w1), ConcreteSInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedUInt(w1.min(w2)) }
        else { ConcreteUInt(v1 % v2, w1.min(w2)) }
      case (ConcreteSInt(v1, w1), ConcreteUInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedSInt(w1.min(w2 + 1)) }
        else { ConcreteSInt(v1 % v2, w1.min(w2 + 1)) }
      case (ConcreteSInt(v1, w1), ConcreteSInt(v2, w2)) =>
        if(that.value == BigInt(0)) { PoisonedSInt(w1.min(w2)) }
        else { ConcreteSInt(v1 % v2, w1.min(w2)) }
    }
  }
  // Comparison operators
  def <(that: Concrete):  ConcreteUInt = ConcreteUInt(boolToBigInt(this.value < that.value), 1)
  def <=(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value <= that.value), 1)
  def >(that: Concrete):  ConcreteUInt = ConcreteUInt(boolToBigInt(this.value > that.value), 1)
  def >=(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value >= that.value), 1)
  // scalastyle:off method.name
  def ==(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value == that.value), 1)
  // scalastyle:on method.name
  def !=(that: Concrete): ConcreteUInt = ConcreteUInt(boolToBigInt(this.value != that.value), 1)
  // Padding
  def pad(n: BigInt): Concrete = pad(n.toInt)
  def pad(n: Int): Concrete = this match {
    case ConcreteUInt(v, w) => ConcreteUInt(this.value, this.width.max(n))
    case ConcreteSInt(v, w) => ConcreteSInt(this.value, this.width.max(n))
  }
  // Casting     TODO: I don't think this is done right, need to look at top bit each way
  def asUInt: ConcreteUInt = {
    this match {
      case si: ConcreteSInt => tail(0)
      case _ => ConcreteUInt(this.value, this.width)
    }
  }
  def asSInt: ConcreteSInt = {
    this match {
      case ConcreteSInt(previousValue, previousWidth) =>
        ConcreteSInt(previousValue, previousWidth)
      case ConcreteUInt(previousValue, previousWidth) =>
        val newValue = {
          if(previousValue == Big1 && previousWidth == 1) {
            BigInt(-1)
          }
          else {
            var signCrossover = BigInt(1) << (previousWidth - 1)
            if(previousValue >= signCrossover) {
              signCrossover <<= 1
              previousValue - signCrossover
            }
            else {
              previousValue
            }
          }
        }
        ConcreteSInt(newValue, this.width)
    }
  }
  def asClock: ConcreteClock = ConcreteClock(boolToBigInt((this.value & BigInt(1)) > BigInt(0)))
  // Shifting
  def <<(that: Concrete): Concrete = that match {
    case ConcreteUInt(thisValue, _) =>
      assert(thisValue >= 0, s"ERROR:$this << $that ${that.value} must be >= 0")
      <<(that.value)
    case _ => throw new InterpreterException(s"Cannot shift $this << $that where $that is not a UInt parameter")
  }
  def <<(that: ConcreteUInt): Concrete = {
    val shift = that.value.toInt
    assert(that.value >= 0, s"ERROR:$this << $that ${that.value} must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value << shift, thisWidth + shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value << shift, thisWidth + shift)
    }
  }
  def <<(that: BigInt): Concrete = <<(that.toInt)
  def <<(shift: Int): Concrete = {
    assert(shift >= 0, s"ERROR:$this << $shift $shift must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value << shift, thisWidth + shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value << shift, thisWidth + shift)
    }
  }
  def >>(that: Concrete): Concrete = that match {
    case ConcreteUInt(thatValue, _) =>
      val shift = thatValue.toInt
      assert(shift >= 0, s"ERROR:$this >> $that ${that.value} must be >= 0")
      assert(shift < this.width, s"ERROR:$this >> $that ${that.value} must be > ${this.width}")
      ConcreteUInt(this.value >> shift, this.width)
    case _ => throw new InterpreterException(s"Cannot shift $this >> $that where $that is not a UInt parameter")
  }
  def >>(that: BigInt): Concrete = >>(that.toInt)
  def >>(shift: Int): Concrete = {
    assert(shift >= 0, s"ERROR:$this >> $shift $shift must be >= 0")
    assert(shift < this.width, s"ERROR:$this >> $shift $shift must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth) => ConcreteUInt(this.value >> shift, thisWidth - shift)
      case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(this.value >> shift, thisWidth - shift)
    }
  }
  // Signed
  def cvt: ConcreteSInt = this match {
    case ConcreteUInt(thisValue, thisWidth) => ConcreteSInt(thisValue, thisWidth + 1)
    case ConcreteSInt(thisValue, thisWidth) => ConcreteSInt(thisValue, thisWidth)
  }
  def neg: ConcreteSInt = {
    //TODO: Is this right?
    ConcreteSInt(-value, width + 1)
  }
  def not: ConcreteUInt = this match {
    case ConcreteUInt(v, _) =>
      var flipped = value
      for(bitIndex <- 0 until width) {
        flipped = flipped.flipBit(bitIndex)
      }
      ConcreteUInt(flipped, width)
    case ConcreteSInt(v, _) =>
      var flipped = value.abs
      for(bitIndex <- 0 until width-1) {
        flipped = flipped.flipBit(bitIndex)
      }
      if(v >= 0) flipped = flipped.setBit(width-1) // invert sign and stick at high end
      ConcreteUInt(flipped, width)
  }
  def &(that: Concrete): ConcreteUInt = ConcreteUInt(this.value & that.value, width.max(that.width))
  def |(that: Concrete): ConcreteUInt = ConcreteUInt(this.value | that.value, width.max(that.width))
  def ^(that: Concrete): ConcreteUInt = ConcreteUInt(this.value ^ that.value, width.max(that.width))
  def cat(that: Concrete): ConcreteUInt = {
    ConcreteUInt((this.value.abs << that.width) + that.value, this.width + that.width)
  }
  // extraction
  def getBits(hi: Int, lo: Int): BigInt = {
    val desiredNumberOfBits = (hi - lo) + 1
    val bottomRemoved = value >> lo
    val modulus = Big1 << desiredNumberOfBits
    val topRemoved = bottomRemoved % modulus
    topRemoved
  }
//  def bits(hi: BigInt, lo: BigInt): ConcreteUInt = bits(hi.toInt, lo.toInt)
  def bits(hi: BigInt, lo: BigInt): Concrete = {
  assert(lo >= Big0, s"Error:Bits($this, hi=$hi, lo=$lo) lo must be >= 0")
  assert(lo <= width, s"Error:Bits($this, hi=$hi, lo=$lo) lo must be < ${this.width}")
  assert(hi >= lo,   s"Error:Bits($this, hi=$hi, lo=$lo) hi must be >= $lo")
  assert(hi <= width, s"Error:Bits($this, hi=$hi, lo=$lo) hi must be < ${this.width}")
    val (high, low) = (hi.toInt, lo.toInt)
    this match {
      case ConcreteUInt(v, _) => ConcreteUInt(getBits(high, low), high - low + 1)
      case ConcreteSInt(v, _) => ConcreteSInt(getBits(high, low), high - low + 1)
    }
  }
  def head(n: BigInt): Concrete = {
    assert(n > 0, s"Error:Head($this, n=$n) n must be >= 0")
    assert(n <= width, s"Error:Head($this, n=$n) n must be <= ${this.width}")
    bits(width-1, width - n)
  }
  def tail(n: BigInt): ConcreteUInt = tail(n.toInt)
  def tail(n: Int): ConcreteUInt = {
    assert(n >= 0, s"Error:Tail($this, n=$n) n must be >= 0")
    assert(n < width, s"Error:Tail($this, n=$n) n must be < ${this.width}")
//    if(n == 0) {
//      ConcreteUInt(value, width)
//    }
//    else {
      var x = Big0
      val bitsWanted = width - n
      for(i <- 0 until bitsWanted) {
        if(value.testBit(i)) x = x.setBit(i)
      }
      ConcreteUInt(x, bitsWanted)
//    }
  }

  def andReduce: Concrete = this match {
    case ConcreteUInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_&&_)), 1)
    case ConcreteSInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_&&_) && (w < Big0)), 1)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(v.testBit(0)), 1)
  }
  def orReduce: Concrete = this match {
    case ConcreteUInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_||_)), 1)
    case ConcreteSInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_||_) || (w < Big0)), 1)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(v.testBit(0)), 1)
  }
  def xorReduce: Concrete = this match {
    case ConcreteUInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_^_)), 1)
    case ConcreteSInt(v, w) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_^_) ^ (w < Big0)), 1)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(! v.testBit(0)), 1)
  }
  def forceWidth(newWidth: Int): Concrete
  def forceWidth(tpe: Type): Concrete
  def asBinaryString: String = {
    val bitString = value.abs.toString(2)

    this match {
      case ConcreteUInt(v, w) =>
        s"UInt<$width>${"0"*(width-bitString.length)}$bitString"
      case ConcreteSInt(v, w) =>
        s"SInt<$width>${if(v<0)"1" else "0"}${"0"*((width-1)-bitString.length)}$bitString"
    }
  }
  def poisoned: Boolean = false
}
object Concrete {
  def apply(u: UIntLiteral): ConcreteUInt = {
    ConcreteUInt(u.value, u.width.asInstanceOf[IntWidth].width.toInt)
  }
  def apply(u: SIntLiteral): ConcreteSInt = {
    ConcreteSInt(u.value, u.width.asInstanceOf[IntWidth].width.toInt)
  }
  def apply(tpe: Type, value: BigInt = Big0): Concrete = {
    tpe match {
      case UIntType(IntWidth(w))     => ConcreteUInt(value, w.toInt)
//      case UIntLiteral(v, IntWidth(w)) => ConcreteUInt(v, w.toInt)
      case SIntType(IntWidth(w))     => ConcreteSInt(value, w.toInt)
//      case SIntLiteral(v, IntWidth(w)) => ConcreteSInt(v, w.toInt)
      case ClockType                 => ConcreteClock(value)
    }
  }
  def randomUInt(width: Int): ConcreteUInt  = ConcreteUInt(randomBigInt(width), width)
  def randomSInt(width: Int): ConcreteSInt  = {
    val (low, high) = extremaOfSIntOfWidth(width)
    val randomValue = randomBigInt(width)
    val positiveRandom = randomValue % ((high - low) + 1)

    ConcreteSInt(positiveRandom + low, width)
  }
  def randomClock():          ConcreteClock = ConcreteClock(randomBigInt(1))
}

/**
  * A runtime instance of a UInt
  *
  * @param value the BigInt value of this UInt, must be non-negative
  * @param width the number of bits in this value, must be big enough to contain value
  */
case class ConcreteUInt(val value: BigInt, val width: Int) extends Concrete {
  if(width < 0) {
    throw new InterpreterException(s"error: ConcreteUInt($value, $width) bad width $width must be > 0")
  }
  val bitsRequired = requiredBitsForUInt(value)
  if((width > 0) && (bitsRequired > width)) {
    throw new InterpreterException(
      s"error: ConcreteUInt($value, $width) bad width $width needs ${requiredBitsForUInt(value.toInt)}"
    )
  }
  def forceWidth(newWidth:Int): ConcreteUInt = {
    if(newWidth == width) this else ConcreteUInt(this.value, newWidth)
  }
  def forceWidth(tpe: Type): ConcreteUInt = forceWidth(typeToWidth(tpe))
  override def toString: String = s"$value.U<$width>"
}
/**
  * A runtime instance of a SInt
  *
  * @param value the BigInt value of this UInt,
  * @param width the number of bits in this value, must be big enough to contain value plus 1 for sign bit
  */
case class ConcreteSInt(val value: BigInt, val width: Int) extends Concrete {
  if(width < 0) {
    throw new InterpreterException(s"error: ConcreteSInt($value, $width) bad width $width must be > 0")
  }
  if(width == 1) {
    if(value < -1 || value > 0) {
      throw new InterpreterException(s"error: ConcreteSInt($value, $width) width one must have value 0 or -1")
    }
  }
  else {
    val bitsRequired = requiredBitsForSInt(value)
    if ((width > 0) && (bitsRequired > width)) {
      throw new InterpreterException(
        s"error: ConcreteSInt($value, $width) bad width $width needs ${requiredBitsForSInt(value)}"
      )
    }
  }

  def forceWidth(newWidth: Int): ConcreteSInt = {
    if(newWidth == width) this else ConcreteSInt(this.value, newWidth)
  }
  def forceWidth(tpe: Type): ConcreteSInt = forceWidth(typeToWidth(tpe))
  override def toString: String = s"$value.S<$width>"
}
case class ConcreteClock(val value: BigInt) extends Concrete {
  val width = 1

  def forceWidth(width: Int): ConcreteClock = {
    if(width == 1) { this }
    else { throw new InterpreterException(s"withWidth($width) not supported for $this") }
  }
  def forceWidth(tpe: Type): ConcreteClock = forceWidth(typeToWidth(tpe))
}

case class PoisonedUInt(width: Int) extends Concrete {
  val value = Big0
  override def forceWidth(w: Int): PoisonedUInt = PoisonedUInt(w)
  def forceWidth(tpe: Type): PoisonedUInt = forceWidth(typeToWidth(tpe))
  override def poisoned: Boolean = true
}
case class PoisonedSInt(width: Int) extends Concrete {
  val value = Big0
  override def forceWidth(w: Int): PoisonedSInt = PoisonedSInt(w)
  def forceWidth(tpe: Type): PoisonedSInt = forceWidth(typeToWidth(tpe))
  override def poisoned: Boolean = true
}



