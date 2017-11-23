// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.WireKind
import firrtl.ir.{Info, IntWidth, NoInfo, UIntType}
import firrtl_interpreter.FirrtlTerp

case class StopOp(
                   info: Info,
                   returnValue: Int,
                   condition: ExpressionResult,
                   parent: FirrtlTerp
                 ) extends Assigner {

  val symbol: Symbol = StopOp.StopOpSymbol

  def run: FuncUnit = {
    val conditionValue = condition match {
      case e: IntExpressionResult => e.apply() > 0
      case e: LongExpressionResult => e.apply() > 0L
      case e: BigExpressionResult => e.apply() > Big(0)
    }
    if (conditionValue) {
      parent.lastStopResult = Some(returnValue)
    }
    () => Unit
  }
}

object StopOp {
  val StopOpSymbol = Symbol("stopop", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
  StopOpSymbol.index = 0
  StopOpSymbol.cardinalNumber = Int.MaxValue
}