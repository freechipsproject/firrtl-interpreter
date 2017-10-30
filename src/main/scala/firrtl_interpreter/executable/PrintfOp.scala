// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.WireKind
import firrtl.ir._

case class PrintfOp(
                     info: Info,
                     string: StringLit,
                     args: Seq[ExpressionResult],
                     condition: ExpressionResult
                   ) extends Assigner {

  val symbol: Symbol = PrintfOp.PrintfOpSymbol

  def run: FuncUnit = {
    val conditionValue = condition match {
      case e: IntExpressionResult  => e.apply() > 0
      case e: LongExpressionResult => e.apply() > 0L
      case e: BigExpressionResult  => e.apply() > Big(0)
    }
    if(conditionValue) {
      val currentArgValues = args.map {
        case e: IntExpressionResult  => e.apply()
        case e: LongExpressionResult => e.apply()
        case e: BigExpressionResult  => e.apply()
      }
      val formatString = string.escape
      printf(formatString, currentArgValues:_*)
    }
    () => Unit
  }
}

object PrintfOp {
  val PrintfOpSymbol = Symbol("printfop", IntSize, UnsignedInt, WireKind, 1, 1, UIntType(IntWidth(1)), NoInfo)
  PrintfOpSymbol.index = 0
  PrintfOpSymbol.cardinalNumber = 0
}