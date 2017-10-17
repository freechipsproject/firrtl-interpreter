// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir.Info
import firrtl_interpreter.FirrtlTerp

case class StopOp(
                   info: Info,
                   returnValue: Int,
                   condition: ExpressionResult,
                   parent: FirrtlTerp
                 ) extends Assigner {

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