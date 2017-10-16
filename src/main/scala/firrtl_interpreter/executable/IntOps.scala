// See LICENSE for license details.

package firrtl_interpreter.executable

trait IntExpressionResult extends ExpressionResult {
  def apply(): Int
}

case class GetIntConstant(n: Int) extends IntExpressionResult {
  def apply(): Int = n
}

case class ToBig(f: FuncInt) extends BigExpressionResult {
  def apply(): Big = Big(f())
}

case class ToLong(f: FuncInt) extends LongExpressionResult {
  def apply(): Long = f().toLong
}

case class LongToBig(f: FuncLong) extends BigExpressionResult {
  def apply(): Big = BigInt(f())
}

case class LongToInt(f: FuncLong) extends IntExpressionResult {
  def apply(): Int = f().toInt
}

case class BigToLong(f: FuncBig) extends LongExpressionResult {
  def apply(): Long = f().toLong
}

case class ToInt(f: FuncBig) extends IntExpressionResult {
  def apply(): Int = f().toInt
}

case class AddInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() + f2()
}

case class SubInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() - f2()
}

case class MulInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() * f2()
}

case class DivInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() / f2()
}

case class RemInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() % f2()
}

case class MuxInts(condition: FuncInt, trueClause: FuncInt, falseClause: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
}

case class EqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() == f2()) 1 else 0
}

case class NeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() != f2()) 1 else 0
}

case class LtInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() < f2()) 1 else 0
}

case class LeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() <= f2()) 1 else 0
}

case class GtInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

case class GeqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() >= f2()) 1 else 0
}

case class AsUIntInts(f1: FuncInt, isSigned: Boolean, width: Int) extends IntExpressionResult {
  private val mask = (1 << width) - 1

//  def apply(): Int = if (isSigned) applySigned() else applyUnsigned()
  def apply(): Int = f1() & mask
}

case class AsSIntInts(f1: FuncInt, isSigned: Boolean, width: Int) extends IntExpressionResult {
  def apply(): Int = if (isSigned) applySigned() else applyUnsigned()

  private val nextPowerOfTwo: Int = 1 << (width - 1)

  def applySigned(): Int = f1()

  def applyUnsigned(): Int = {
    val uInt = f1()
    if (width == 1 && uInt == 1) {
      -1
    }
    else if ((uInt & nextPowerOfTwo) > 0) {
      nextPowerOfTwo - uInt
    } else {
      uInt
    }
  }
}

case class AsClockInts(f1: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() == 0) 0 else 1
}

case class ShlInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
}

case class ShrInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
}

case class DshlInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() << f2()
}

case class DshrInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() >> f2()
}

case class NegInts(f1: FuncInt) extends IntExpressionResult {
  def apply(): Int = - f1()
}

case class NotInts(f1: FuncInt) extends IntExpressionResult {
  def apply(): Int = ~ f1()
}

case class AndInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() & f2()
}

case class OrInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() | f2()
}

case class XorInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() ^ f2()
}

/**
  * are all bits set
  * @param f1 value to be `and` reduced
  * @param width result bit size
  */
case class AndrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
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
  * @param width result bit size  */
case class OrrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    if(f1() != 0) { 0 } else { 1 }
  }
}

/**
  * are all bits set
  * @param f1 value to be `xor` reduced
  * @param width result bit size  */
case class XorrInts(f1: FuncInt, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val uInt = f1()
    (0 until width).map(n => (uInt >> n) & 1).reduce(_ ^ _)
  }
}

case class CatInts(
                    f1: FuncInt, f1IsSigned: Boolean, f1Width: Int,
                    f2: FuncInt, f2IsSigned: Boolean, f2Width: Int
                  ) extends IntExpressionResult {
  def apply(): Int = {
    (AsUIntInts(f1, f1IsSigned, f1Width)() << f2Width) | AsUIntInts(f2, f2IsSigned, f2Width)()
  }
}

case class BitsInts(f1: FuncInt, isSigned: Boolean, high: Int, low: Int, originalWidth: Int)
  extends IntExpressionResult {
  private val mask = (1 << ((high - low) + 1)) - 1

  def apply(): Int = {
    val uInt = AsUIntInts(f1, isSigned, originalWidth).apply()
    (uInt >> low) & mask
  }
}

case class HeadInts(f1: FuncInt, isSigned: Boolean, takeBits: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask = (1 << takeBits) - 1
  private val shift = originalWidth - takeBits

  def apply(): Int = {
    val uInt = AsUIntInts(f1, isSigned, originalWidth).apply()
    (uInt >> shift) & mask
  }
}

case class TailInts(f1: FuncInt, isSigned: Boolean, toDrop: Int, originalWidth: Int) extends IntExpressionResult {
  private val mask: Int = (1 << (originalWidth - toDrop)) - 1

  def apply(): Int = {
    val f1Value = f1()
    val uInt = AsUIntInts(f1, isSigned, originalWidth).apply()
    uInt & mask
  }
}

case class UndefinedInts(width: Int) {
  val maxValue: Int = 1 << width
  def apply(): Int = util.Random.nextInt(maxValue)
}

