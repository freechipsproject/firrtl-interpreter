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

case class TailBigs(f1: FuncBig, isSigned: Boolean, dropNumber: Int, width: Int) extends BigExpressionResult {
  def apply(): Big = {
    val int = f1()
    int.abs & ((1 << (width - dropNumber)) - 1)
  }
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
  val apply = if(isSigned) applySigned else applyUnsigned
  def applySigned(): Big = TailBigs(f1, isSigned, 1, width).apply()
  def applyUnsigned(): Big = f1()
}

case class AsSIntBigs(f1: FuncBig, isSigned: Boolean, width: Int) extends BigExpressionResult {
  val apply = if(isSigned) applySigned else applyUnsigned
  val mask: Big = BigInt(1) << (width - 1)
  def applySigned(): Big = f1()
  def applyUnsigned(): Big = {
    val uInt = f1()
    if(width == 1 && uInt == BigInt(1)) {
      BigInt(-1)
    }
    else if((uInt & mask) > 0) {
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

case class AssignBig(uBig: BigValue, expression: FuncBig) extends Assigner {
  def apply(): Unit = {
    uBig.value = expression()
  }
}


