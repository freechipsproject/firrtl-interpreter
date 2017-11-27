// See LICENSE for license details.

package firrtl_interpreter.executable

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
  private val mask = LongUtils.makeMask(width)

  //  def apply(): Int = if (isSigned) applySigned() else applyUnsigned()
  def apply(): Long = f1() & mask
}

case class AsSIntLongs(f1: FuncLong, isSigned: Boolean, width: Int) extends LongExpressionResult {
  def apply(): Long = if (isSigned) applySigned() else applyUnsigned()

  val nextPowerOfTwo: Long = 1L << (width - 1)

  def applySigned(): Long = f1()

  def applyUnsigned(): Long = {
    val uInt = f1()
    if (width == 1 && uInt == 1L) {
      -1L
    }
    else if ((uInt & nextPowerOfTwo) > 0) {
      nextPowerOfTwo - uInt
    } else {
      uInt
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
  def apply(): Long = f1() >> f2().toInt
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
  * @param width result bit size
  */
case class AndrLongs(f1: FuncLong, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    var uInt = f1() << 1
    if((0 until width).exists { _ =>
      uInt >>= 1
      (uInt & 1) == 0
    }) { 0 } else { 1 }
  }
}

/**
  * are any bits set
  * @param f1 value to be `or` reduced
  * @param width result bit size
  */
case class OrrLongs(f1: FuncLong, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    if(f1() != 0) { 0 } else { 1 }
  }
}

/**
  * are all bits set
  * @param f1 value to be `xor` reduced
  * @param width result bit size
  */
case class XorrLongs(f1: FuncLong, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val uInt = f1()
    (0 until width).map(n => ((uInt >> n) & 1L).toInt).reduce(_ ^ _)
  }
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

