// See LICENSE for license details.

package firrtl_interpreter.executable

trait BigExpressionResult extends ExpressionResult {
  def apply(): Big
}

case class GetBigConstant(n: Big) extends BigExpressionResult {
  def apply(): Big = n
}

case class GetBig(uBig: BigValue) extends BigExpressionResult {
  val apply: FuncBig = {
    if(true) nakedGetBig else verboseGetBig
  }

  def nakedGetBig(): Big = {
    uBig.value
  }
  def verboseGetBig(): Big = {
    println(s"getting int from index ${nakedGetBig()}")
    nakedGetBig()
  }
}

case class AddBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() + f2()
}

case class SubBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1() - f2()
}

case class TailBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = f1()
}

case class MuxBigs(condition: FuncBig, trueClause: FuncBig, falseClause: FuncBig) extends BigExpressionResult {
  def apply(): Big = if(condition() > 0) trueClause() else falseClause()
}

case class EqBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = if(f1() == f2()) 1 else 0
}

case class GtBigs(f1: FuncBig, f2: FuncBig) extends BigExpressionResult {
  def apply(): Big = if(f1() > f2()) 1 else 0
}

case class AssignBig(uBig: BigValue, expression: FuncBig) extends Assigner {
  def apply(): Unit = {
//    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
    uBig.value = expression()
  }
}


