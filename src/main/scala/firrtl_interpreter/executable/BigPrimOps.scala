// See LICENSE for license details.

package firrtl_interpreter.executable

trait BigExpressionResult extends ExpressionResult {
  def apply(): Big
}

case class GetBigConstant(n: Big) extends BigExpressionResult {
  def apply(): Big = n
}

case class GetBig(uBig: BigValue) extends BigExpressionResult {
  def apply(): Big = {
    uBig.value
  }
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
  private val mask = Big(1) << (width - 1)
  def apply(): Big = if(isSigned) applySigned() else applyUnsigned()
  def applySigned(): Big = {
    val sInt = f1()
    if(sInt < 0) {
      (mask + sInt) | mask
    }
    else {
      sInt
    }
  }
  def applyUnsigned(): Big = f1()
}

case class AsSIntBigs(f1: FuncBig, isSigned: Boolean, width: Int) extends BigExpressionResult {
  def apply(): Big = if(isSigned) applySigned() else applyUnsigned()
  val mask: Big = BigInt(1) << (width - 1)
  def applySigned(): Big = f1()
  def applyUnsigned(): Big = {
    val uInt = f1()
    if(width == 1 && uInt == BigInt(1)) {
      BigInt(-1)
    }
    else if((uInt & mask) >= 0) {
      uInt
    } else {
      uInt - mask
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
  def apply(): Big = f1() << f2().toInt
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

case class CatBigs(f1: FuncBig, f2: FuncBig, width2: Int) extends BigExpressionResult {
  def apply(): Big = (f1() << width2) | f2()
}

case class BitsBigs(f1: FuncBig, isSigned: Boolean, width: Int, high: Int, low: Int) extends BigExpressionResult {
  val mask = BigInt((1 << ((high - low) + 1)) - 1)

  def apply(): Big = {
    val uInt = AsUIntBigs(f1, isSigned, width).apply()
    (uInt >> low) & mask
  }
}

case class HeadBigs(f1: FuncBig, isSigned: Boolean, high: Int, width: Int) extends BigExpressionResult {
  val mask = BigInt((1 << (high + 1)) - 1)

  def apply(): Big = {
    val uInt = AsUIntBigs(f1, isSigned, width).apply()
    uInt & mask
  }
}

case class TailBigs(f1: FuncBig, isSigned: Boolean, toDrop: Int, width: Int) extends BigExpressionResult {
  val high: Int = width - toDrop
  val mask = BigInt((1 << (high + 1)) - 1)

  def apply(): Big = {
    val uInt = AsUIntBigs(f1, isSigned, width).apply()
    uInt & mask
  }
}

case class AssignBig(uBig: BigValue, expression: FuncBig) extends Assigner {
  def apply(): Unit = {
    uBig.value = expression()
  }
}


