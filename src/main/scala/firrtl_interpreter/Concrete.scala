// See LICENSE for license details.
package firrtl_interpreter

import firrtl.ir._

// scalastyle:off number.of.methods
trait Concrete {
  val value : BigInt
  val width : Int
  val lowBitOffset = 0

  def poisoned: Boolean
  def poisonString: String = if(poisoned) "P" else ""
  def poison(p1: Boolean, p2: Boolean) : Boolean = p1 || p2

  def +(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) =>
        ConcreteUInt(v1 + v2, w1.max(w2) + 1, poison(p1, p2))
      case (ConcreteUInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) =>
        ConcreteSInt(v1 + v2, w1.max(w2 - 1) + 2, poison(p1, p2))
      case (ConcreteSInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) =>
        ConcreteSInt(v1 + v2, w2.max(w1 - 1) + 2, poison(p1, p2))
      case (ConcreteSInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) =>
        ConcreteSInt(v1 + v2, w1.max(w2) + 1, poison(p1, p2))
    }
  }
  def -(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) =>
        ConcreteSInt(v1 - v2, w1.max(w2) + 1, poison(p1, p2))
      case (ConcreteUInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) =>
        val newWidth = (w1 + 2).max(w2 + 1)
        ConcreteSInt(v1 - v2, newWidth, poison(p1, p2))
      case (ConcreteSInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) =>
        val newWidth = if(w1 == 1) w2 + 1 else (w1 + 1).max(w2 + 2)
        ConcreteSInt(v1 - v2, newWidth, poison(p1, p2))
      case (ConcreteSInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) =>
        ConcreteSInt(v1 - v2, w1.max(w2) + 1, poison(p1, p2))
    }
  }
  def *(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) => ConcreteUInt(v1 * v2, w1 + w2, poison(p1, p2))
      case (ConcreteUInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) => ConcreteSInt(v1 * v2, w1 + w2, poison(p1, p2))
      case (ConcreteSInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) => ConcreteSInt(v1 * v2, w1 + w2, poison(p1, p2))
      case (ConcreteSInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) => ConcreteSInt(v1 * v2, w1 + w2, poison(p1, p2))
    }
  }
  def /(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedUInt(w1) }
        else { ConcreteUInt(v1 / v2, w1) }
      case (ConcreteUInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedSInt(w1 + 1) }
        else { ConcreteSInt(v1 / v2, w1 + 1) }
      case (ConcreteSInt(v1, w1, p1), ConcreteUInt(v2, w2, p2)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedSInt(w1) }
        else { ConcreteSInt(v1 / v2, w1) }
      case (ConcreteSInt(v1, w1, p1), ConcreteSInt(v2, w2, p2)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedSInt(w1 + 1) }
        else { ConcreteSInt(v1 / v2, w1 + 1) }
    }
  }
  def %(that: Concrete): Concrete = {
    (this, that) match {
      case (ConcreteUInt(v1, w1, _), ConcreteUInt(v2, w2, _)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedUInt(w1.min(w2)) }
        else { ConcreteUInt(v1 % v2, w1.min(w2)) }
      case (ConcreteUInt(v1, w1, _), ConcreteSInt(v2, w2, _)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedUInt(w1.min(w2)) }
        else { ConcreteUInt(v1 % v2, w1.min(w2)) }
      case (ConcreteSInt(v1, w1, _), ConcreteUInt(v2, w2, _)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedSInt(w1.min(w2 + 1)) }
        else { ConcreteSInt(v1 % v2, w1.min(w2 + 1)) }
      case (ConcreteSInt(v1, w1, _), ConcreteSInt(v2, w2, _)) =>
        if(that.value == BigInt(0)) { Concrete.poisonedSInt(w1.min(w2)) }
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
    case ConcreteUInt(v, w, p) => ConcreteUInt(this.value, this.width.max(n), p)
    case ConcreteSInt(v, w, p) => ConcreteSInt(this.value, this.width.max(n), p)
  }
  // Casting     TODO: I don't think this is done right, need to look at top bit each way
  def asUInt: ConcreteUInt = {
    this match {
      case si: ConcreteSInt => tail(0)
      case _ => ConcreteUInt(this.value, this.width, this.poisoned)
    }
  }
  def asSInt: ConcreteSInt = {
    this match {
      case ConcreteSInt(previousValue, previousWidth, p) =>
        ConcreteSInt(previousValue, previousWidth, p)
      case ConcreteUInt(previousValue, previousWidth, p) =>
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
        ConcreteSInt(newValue, this.width, p)
    }
  }
  def asClock: ConcreteClock = ConcreteClock(boolToBigInt((this.value & BigInt(1)) > BigInt(0)))
  // Shifting
  def <<(that: Concrete): Concrete = that match {
    case ConcreteUInt(thisValue, _, _) =>
      assert(thisValue >= 0, s"ERROR:$this << $that ${that.value} must be >= 0")
      <<(that.value)
    case _ => throw new InterpreterException(s"Cannot shift $this << $that where $that is not a UInt parameter")
  }
  def <<(that: ConcreteUInt): Concrete = {
    val shift = that.value.toInt
    assert(that.value >= 0, s"ERROR:$this << $that ${that.value} must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth, p) => ConcreteUInt(this.value << shift, thisWidth + shift, p)
      case ConcreteSInt(thisValue, thisWidth, p) => ConcreteSInt(this.value << shift, thisWidth + shift, p)
    }
  }
  def <<(that: BigInt): Concrete = <<(that.toInt)
  def <<(shift: Int): Concrete = {
    assert(shift >= 0, s"ERROR:$this << $shift $shift must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth, p) => ConcreteUInt(this.value << shift, thisWidth + shift, p)
      case ConcreteSInt(thisValue, thisWidth, p) => ConcreteSInt(this.value << shift, thisWidth + shift, p)
    }
  }
  def >>(that: Concrete): Concrete = that match {
    case ConcreteUInt(thatValue, _, _) =>
      val shift = thatValue.toInt
      assert(shift >= 0, s"ERROR:$this >> $that ${that.value} must be >= 0")
      this match {
        case _: ConcreteUInt => ConcreteUInt(this.value >> shift, this.width)
        case _: ConcreteSInt => ConcreteSInt(this.value >> shift, this.width)
      }
    case _ => throw new InterpreterException(s"Cannot shift $this >> $that where $that is not a UInt parameter")
  }
  def >>(that: BigInt): Concrete = >>(that.toInt)
  def >>(shift: Int): Concrete = {
    assert(shift >= 0, s"ERROR:$this >> $shift $shift must be >= 0")
    this match {
      case ConcreteUInt(thisValue, thisWidth, p) => ConcreteUInt(this.value >> shift, thisWidth - shift, p)
      case ConcreteSInt(thisValue, thisWidth, p) => ConcreteSInt(this.value >> shift, thisWidth - shift, p)
    }
  }
  // Signed
  def cvt: ConcreteSInt = this match {
    case ConcreteUInt(thisValue, thisWidth, p) => ConcreteSInt(thisValue, thisWidth + 1, p)
    case ConcreteSInt(thisValue, thisWidth, p) => ConcreteSInt(thisValue, thisWidth, p)
  }
  def neg: ConcreteSInt = {
    //TODO: Is this right?
    ConcreteSInt(-value, width + 1)
  }
  def not: ConcreteUInt = this match {
    case _ : ConcreteUInt | _ : ConcreteSInt =>
      val uInt = this.asUInt
      var flipped = uInt.value
      for(bitIndex <- 0 until uInt.width) {
        flipped = flipped.flipBit(bitIndex)
      }
      ConcreteUInt(flipped, width, poisoned)
  }
  def &(that: Concrete): ConcreteUInt = {
    ConcreteUInt(this.asUInt.value & that.asUInt.value, width.max(that.width), poison(this.poisoned, that.poisoned))
  }
  def |(that: Concrete): ConcreteUInt = {
    ConcreteUInt(this.asUInt.value | that.asUInt.value, width.max(that.width), poison(this.poisoned, that.poisoned))
  }
  def ^(that: Concrete): ConcreteUInt = {
    ConcreteUInt(this.asUInt.value ^ that.asUInt.value, width.max(that.width), poison(this.poisoned, that.poisoned))
  }
  def cat(that: Concrete): ConcreteUInt = {
    ConcreteUInt((this.asUInt.value << that.width) + that.asUInt.value, this.width + that.width,
      poison(this.poisoned, that.poisoned))
  }
  // extraction
  def getBits(hi: Int, lo: Int): BigInt = {
    val uint = this.asUInt
    val desiredNumberOfBits = (hi - lo) + 1
    val bottomRemoved = uint.value >> lo
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
      case ConcreteUInt(v, _, p) => ConcreteUInt(getBits(high, low), high - low + 1, p)
      case ConcreteSInt(v, _, p) => ConcreteUInt(getBits(high, low), high - low + 1, p)
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
      ConcreteUInt(x, bitsWanted, this.poisoned)
//    }
  }

  def andReduce: Concrete = this match {
    case ConcreteUInt(v, w, p) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_&&_)), 1, p)
    case ConcreteSInt(v, w, p) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_&&_) && (w < Big0)), 1, p)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(v.testBit(0)), 1)
  }
  def orReduce: Concrete = this match {
    case ConcreteUInt(v, w, p) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_||_)), 1, p)
    case ConcreteSInt(v, w, p) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_||_) || (w < Big0)), 1, p)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(v.testBit(0)), 1)
  }
  def xorReduce: Concrete = this match {
    case ConcreteUInt(v, w, p) =>
      ConcreteUInt(boolToBigInt((0 until w).map(i => v.testBit(i)).reduce(_^_)), 1, p)
    case ConcreteSInt(v, w, p) =>
      ConcreteUInt(boolToBigInt((0 until w-1).map(i => v.testBit(i)).reduce(_^_) ^ (w < Big0)), 1, p)
    case ConcreteClock(v) =>
      ConcreteUInt(boolToBigInt(! v.testBit(0)), 1)
  }
  def forceWidth(newWidth: Int): Concrete
  def forceWidth(tpe: Type): Concrete

  @deprecated(s"Use toBinaryString instead")
  def asBinaryString: String = {
    val bitString = value.abs.toString(2)

    this match {
      case ConcreteUInt(v, w, p) =>
        s"${poisonString}UInt<$width>${"0"*(width-bitString.length)}$bitString"
      case ConcreteSInt(v, w, p) =>
        s"${poisonString}SInt<$width>${if(v<0)"1" else "0"}${"0"*((width-1)-bitString.length)}$bitString"
    }
  }
  /**
    * Show just the bit value of this concrete, string is left padded with zeros to match width
    * This functions does not include type and poison information, it's just the bits.
    * @return
    */
  def toBinaryString: String = {
    val bitString = {
      val v = if(value < Big0) {
        (value + (BigInt(1) << width)).toString(2)
      }
      else {
        value.toString(2)
      }
      ("0" * (width - v.length)) + v
    }

    bitString
  }

  /**
    * Show just the bit value of this concrete, string is left padded with zeros to match width
    * @return
    */
  def toHexString: String = {
    val hexString = {
      val v = if(value < Big0) {
        (value + (BigInt(1) << width)).toString(16)
      }
      else {
        value.toString(16)
      }
      ("0" * ((width / 4) - v.length)) + v
    }

    hexString
  }

  def showValue: String = {
    def showPoison: String = if(poisoned) "☠" else ""
    s"$showPoison $value$showPoison"
  }
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
      case other =>
        throw InterpreterException(s"Cannot create value for unsupported type: $other")
    }
  }
  def poisonedUInt(width: Int): ConcreteUInt = randomUInt(width, poisoned = true)
  def poisonedSInt(width: Int): ConcreteSInt = randomSInt(width, poisoned = true)

  def randomUInt(width: Int, poisoned: Boolean = false): ConcreteUInt  = {
    ConcreteUInt(randomBigInt(width), width, poisoned)
  }
  def randomSInt(width: Int, poisoned: Boolean = false): ConcreteSInt  = {
    val (low, high) = extremaOfSIntOfWidth(width)
    val randomValue = randomBigInt(width)
    val positiveRandom = randomValue % ((high - low) + 1)

    ConcreteSInt(positiveRandom + low, width, poisoned)
  }
  def randomClock():          ConcreteClock = ConcreteClock(randomBigInt(1))
}

/**
  * A runtime instance of a UInt
  *
  * @param value the BigInt value of this UInt, must be non-negative
  * @param width the number of bits in this value, must be big enough to contain value
  */
case class ConcreteUInt(value: BigInt, width: Int, poisoned: Boolean = false) extends Concrete {
  if(width < 0) {
    throw new InterpreterException(s"error: ConcreteUInt($value, $width) bad width $width must be > 0")
  }
  if(value < 0) {
    throw new InterpreterException(s"error: ConcreteUInt($value, $width) bad value $value must be >= 0")
  }
  val bitsRequired: Int = requiredBitsForUInt(value)
  if((width > 0) && (bitsRequired > width)) {
    throw new InterpreterException(
      s"error: ConcreteUInt($value, $width) bad width $width needs ${requiredBitsForUInt(value)}"
    )
  }
  def forceWidth(newWidth:Int): ConcreteUInt = {
    if(newWidth == width) this else ConcreteUInt(this.value, newWidth)
  }
  def forceWidth(tpe: Type): ConcreteUInt = forceWidth(typeToWidth(tpe))
  override def toString: String = s"$value.${poisonString}U<$width>"
}
/**
  * A runtime instance of a SInt
  *
  * @param value the BigInt value of this UInt,
  * @param width the number of bits in this value, must be big enough to contain value plus 1 for sign bit
  */
case class ConcreteSInt(value: BigInt, width: Int, poisoned: Boolean = false) extends Concrete {
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
    if(newWidth == width) this else ConcreteSInt(this.value, newWidth, poisoned)
  }
  def forceWidth(tpe: Type): ConcreteSInt = forceWidth(typeToWidth(tpe))
  override def toString: String = s"$value.${poisonString}S<$width>"
}
case class ConcreteClock(value: BigInt) extends Concrete {
  val width = 1
  val poisoned = false

  def forceWidth(width: Int): ConcreteClock = {
    if(width == 1) { this }
    else { throw new InterpreterException(s"withWidth($width) not supported for $this") }
  }
  def forceWidth(tpe: Type): ConcreteClock = forceWidth(typeToWidth(tpe))
}
