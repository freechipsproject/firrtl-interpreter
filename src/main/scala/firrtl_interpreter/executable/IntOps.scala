// See LICENSE for license details.

package firrtl_interpreter.executable

trait IntExpressionResult extends ExpressionResult {
  def apply(): Int
}

case class GetIntConstant(n: Int) extends IntExpressionResult {
  def apply(): Int = n
}

case class GetInt(uInt: IntValue) extends IntExpressionResult {
  def apply(): Int = {
    uInt.value
  }
}

case class ToBig(f: FuncInt) extends BigExpressionResult {
  def apply(): Big = Big(f())
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

case class TailInts(f1: FuncInt, isSigned: Boolean, dropNumber: Int, width: Int) extends IntExpressionResult {
  def apply(): Int = {
    val int = f1()
    int.abs & ((1 << (width - dropNumber)) - 1)
  }
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
  val apply = if(isSigned) applySigned else applyUnsigned
  def applySigned(): Int = TailInts(f1, isSigned, 1, width).apply()
  def applyUnsigned(): Int = f1()
}

case class AsSIntInts(f1: FuncInt, isSigned: Boolean, width: Int) extends IntExpressionResult {
  val apply = if(isSigned) applySigned else applyUnsigned
  val mask: Int = 1 << (width - 1)
  def applySigned(): Int = f1()
  def applyUnsigned(): Int = {
    val uInt = f1()
    if(width == 1 && uInt == 1) {
      -1
    }
    else if((uInt & mask) > 0) {
      uInt
    } else {
      uInt - mask
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

case class AssignInt(uInt: IntValue, expression: FuncInt) extends Assigner {
  def apply(): Unit = {
//    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
    uInt.value = expression()
  }
}
