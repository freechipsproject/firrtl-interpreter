// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter.utils.BitMasks

trait LongExpressionResult extends ExpressionResult {
  def apply(): Long
}

case class GetLongConstant(n: Long) extends LongExpressionResult {
  def apply(): Long = n
}

case class AddLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() + f2()
}

case class SubLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() - f2()
}

case class MulLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() * f2()
}

case class DivLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() / f2()
}

case class RemLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() % f2()
}

case class MuxLongs(condition: FuncInt, trueClause: FuncLong, falseClause: FuncLong) extends LongExpressionResult {
  def apply(): Long = if(condition() > 0) trueClause() else falseClause()
}

case class EqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() == f2()) 1 else 0
}
case class NeqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() != f2()) 1 else 0
}

case class LtLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() < f2()) 1 else 0
}

case class LeqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() <= f2()) 1 else 0
}

case class GtLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

case class GeqLongs(f1: FuncLong, f2: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() >= f2()) 1 else 0
}

case class AsUIntLongs(f1: FuncLong, isSigned: Boolean, width: Int) extends LongExpressionResult {
  private val bitMasks = BitMasks.getBitMasksLongs(width)

  def apply(): Long = f1() & bitMasks.allBitsMask
}

case class AsSIntLongs(f1: FuncLong, isSigned: Boolean, width: Int) extends LongExpressionResult {
  private val bitMasks = BitMasks.getBitMasksLongs(width)

  def apply(): Long = {
    val value = f1()
    if(value < 0) {
      value
    }
    else {
      if(bitMasks.isMsbSet(value)) {
        (value & bitMasks.allBitsMask) - bitMasks.nextPowerOfTwo
      }
      else {
        value & bitMasks.allBitsMask
      }
    }
  }
}

case class AsClockLongs(f1: FuncLong) extends IntExpressionResult {
  def apply(): Int = if(f1() == 0) 0 else 1
}

case class ShlLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() << f2().toInt
}

case class ShrLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() >> f2().toInt
}

case class DshlLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() << f2().toInt
}

case class DshrLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = {
    val a: Long = f1()
    val b: Long = f2()
    a >> b.toInt
  }
}

case class NegLongs(f1: FuncLong) extends LongExpressionResult {
  def apply(): Long = - f1()
}

case class NotLongs(f1: FuncLong) extends LongExpressionResult {
  def apply(): Long = ~ f1()
}

case class AndLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() & f2()
}

case class OrLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() | f2()
}

case class XorLongs(f1: FuncLong, f2: FuncLong) extends LongExpressionResult {
  def apply(): Long = f1() ^ f2()
}

/**
  * are all bits set
  * @param f1 value to be `and` reduced
  * @param isSigned is input an SInt
  * @param width result bit size
  */
case class AndrLongs(f1: FuncLong, isSigned: Boolean, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val uInt = AsUIntLongs(f1, isSigned, width).apply()
    var mask = Big(1)
    var bitPosition = 0
    while(bitPosition < width) {
      if((mask & uInt) == 0L) return 0
      mask <<= 1
      bitPosition += 1
    }
    1
  }
}

/**
  * are any bits set
  * @param f1 value to be `or` reduced
  * @param isSigned is input an SInt
  * @param width result bit size
  */
case class OrrLongs(f1: FuncLong, isSigned: Boolean, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val uInt = AsUIntLongs(f1, isSigned, width).apply()
    if(uInt > 0) { 1 } else { 0 }
  }
}

/**
  * are all bits set
  * @param f1 value to be `xor` reduced
  * @param isSigned is input an SInt
  * @param width result bit size
  */
case class XorrLongs(f1: FuncLong, isSigned: Boolean, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val uInt = AsUIntLongs(f1, isSigned, width).apply()
    (0 until width).map(n => ((uInt >> n) & BigInt(1)).toInt).reduce(_ ^ _)  }
}

case class CatLongs(
                    f1: FuncLong, f1IsSigned: Boolean, f1Width: Int,
                    f2: FuncLong, f2IsSigned: Boolean, f2Width: Int
                  ) extends LongExpressionResult {
  def apply(): Long = {
    (AsUIntLongs(f1, f1IsSigned, f1Width)() << f2Width) | AsUIntLongs(f2, f2IsSigned, f2Width)()
  }
}

case class BitsLongs(f1: FuncLong, isSigned: Boolean, high: Int, low: Int, originalWidth: Int)
  extends LongExpressionResult {
  private val mask = LongUtils.makeMask((high - low) + 1)

  def apply(): Long = {
    val uInt = AsUIntLongs(f1, isSigned, originalWidth).apply()
    (uInt >> low) & mask
  }
}

case class HeadLongs(f1: FuncLong, isSigned: Boolean, takeBits: Int, originalWidth: Int) extends LongExpressionResult {
  private val mask = LongUtils.makeMask(takeBits)
  private val shift = originalWidth - takeBits

  def apply(): Long = {
    val uBig = AsUIntLongs(f1, isSigned, originalWidth).apply()
    (uBig >> shift) & mask
  }
}

case class TailLongs(f1: FuncLong, isSigned: Boolean, toDrop: Int, originalWidth: Int) extends LongExpressionResult {
  private val mask: Long = LongUtils.makeMask(originalWidth - toDrop)

  def apply(): Long = {
    val uInt = AsUIntLongs(f1, isSigned, originalWidth).apply()
    uInt & mask
  }
}

case class UndefinedLongs(width: Int) {
  val mask: Long = LongUtils.makeMask(width)
  def apply(): Long = util.Random.nextLong() & mask
}

object LongUtils {
  def makeMask(width: Int): Long = {
    (1L << width) - 1
  }
}

