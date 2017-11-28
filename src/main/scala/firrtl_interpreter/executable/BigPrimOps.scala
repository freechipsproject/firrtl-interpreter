// See LICENSE for license details.

package firrtl_interpreter.executable

trait BigExpressionResult extends ExpressionResult {
  def apply(): Big
}

case class GetBigConstant(n: Big) extends BigExpressionResult {
  def apply(): Big = n
}

case class AddBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() + f2()
}

case class SubBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() - f2()
}

case class MulBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() * f2()
}

case class DivBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() / f2()
}

case class RemBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() % f2()
}

case class MuxBigs(condition: FuncInt, trueClause: FuncBig, falseClause: FuncBig) extends BigExpressionResult {
  def apply(): Big = if(condition() > 0) trueClause() else falseClause()
}

case class EqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() == f2()) 1 else 0
}
case class NeqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() != f2()) 1 else 0
}

case class LtBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() < f2()) 1 else 0
}

case class LeqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() <= f2()) 1 else 0
}

case class GtBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

case class GeqBigs(f1: FuncBig, f2: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() >= f2()) 1 else 0
}

case class AsUIntBigs(f1: FuncBig, isSigned: Boolean, width: Int) extends BigExpressionResult {
  private val nextPowerOfTwo : Long  = 1L << width

  def apply(): BigInt = {
    if(! isSigned) {
      f1()
    }
    else {
      val value = f1()
      if(value < Big(0)) {
        value + nextPowerOfTwo
      }
      else {
        value
      }
    }
  }
}

case class AsSIntBigs(f1: FuncBig, isSigned: Boolean, width: Int) extends BigExpressionResult {
  def apply(): Big = if (isSigned) applySigned() else applyUnsigned()

  private val nextPowerOfTwo = Big(1) << width
  private val msbMask        = Big(1) << (width - 1)

  def applySigned(): Big = f1()

  def applyUnsigned(): Big = {
    val value = f1()
    if (width == 1 && value == Big(1)) {
      -1L
    }
    else {
      val result = if((value & msbMask) > 0) {
        value - nextPowerOfTwo
      }
      else {
        value
      }
      result
    }
  }
}

case class AsClockBigs(f1: FuncBig) extends IntExpressionResult {
  def apply(): Int = if(f1() == 0) 0 else 1
}

case class ShlBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() << f2().toInt
}

case class ShrBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() >> f2().toInt
}

case class DshlBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() << f2().toInt
}

case class DshrBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() >> f2().toInt
}

case class NegBigs(f1: FuncBig) extends BigExpressionResult {
  def apply(): Big = - f1()
}

case class NotBigs(f1: FuncBig) extends BigExpressionResult {
  def apply(): Big = ~ f1()
}

case class AndBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() & f2()
}

case class OrBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() | f2()
}

case class XorBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() ^ f2()
}

/**
  * are all bits set
  * @param f1 value to be `and` reduced
  * @param width result bit size
  */
case class AndrBigs(f1: FuncBig, width: Int) extends IntExpressionResult {
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
case class OrrBigs(f1: FuncBig, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    if(f1() != 0) { 0 } else { 1 }
  }
}

/**
  * are all bits set
  * @param f1 value to be `xor` reduced
  * @param width result bit size
  */
case class XorrBigs(f1: FuncBig, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val uInt = f1()
    (0 until width).map(n => ((uInt >> n) & BigInt(1)).toInt).reduce(_ ^ _)
  }
}

case class CatBigs(
                    f1: FuncBig, f1IsSigned: Boolean, f1Width: Int,
                    f2: FuncBig, f2IsSigned: Boolean, f2Width: Int
                  ) extends BigExpressionResult {
  def apply(): Big = {
    (AsUIntBigs(f1, f1IsSigned, f1Width)() << f2Width) | AsUIntBigs(f2, f2IsSigned, f2Width)()
  }
}

case class BitsBigs(f1: FuncBig, isSigned: Boolean, high: Int, low: Int, originalWidth: Int)
  extends BigExpressionResult {
  private val mask = Big.makeMask((high - low) + 1)

  def apply(): Big = {
    val uInt = AsUIntBigs(f1, isSigned, originalWidth).apply()
    (uInt >> low) & mask
  }
}

case class HeadBigs(f1: FuncBig, isSigned: Boolean, takeBits: Int, originalWidth: Int) extends BigExpressionResult {
  private val mask = Big.makeMask(takeBits)
  private val shift = originalWidth - takeBits

  def apply(): Big = {
    val uBig = AsUIntBigs(f1, isSigned, originalWidth).apply()
    (uBig >> shift) & mask
  }
}

case class TailBigs(f1: FuncBig, isSigned: Boolean, toDrop: Int, originalWidth: Int) extends BigExpressionResult {
  private val mask: Big = Big.makeMask(originalWidth - toDrop)

  def apply(): Big = {
    val uInt = AsUIntBigs(f1, isSigned, originalWidth).apply()
    uInt & mask
  }
}

case class UndefinedBigs(width: Int) {
  def apply(): Big = BigInt(width, util.Random)
}

