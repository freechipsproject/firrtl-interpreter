// See LICENSE for license details.

package firrtl_interpreter.executable

trait IntExpressionResult extends ExpressionResult {
  def apply(): Int
}

case class GetIntConstant(n: Int) extends IntExpressionResult {
  def apply(): Int = n
}

case class GetInt(uInt: IntValue) extends IntExpressionResult {
  val apply: FuncInt = {
    if(true) nakedGetInt else verboseGetInt
  }

  def nakedGetInt(): Int = {
    uInt.value
  }
  def verboseGetInt(): Int = {
    println(s"getting int from index ${nakedGetInt()}")
    nakedGetInt()
  }
}

case class ToBig(f: FuncInt) {
  def apply(): Big = Big(f())
}

case class AddInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() + f2()
}

case class SubInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1() - f2()
}

case class TailInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = f1()
}

case class MuxInts(condition: FuncInt, trueClause: FuncInt, falseClause: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
}

case class EqInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() == f2()) 1 else 0
}

case class GtInts(f1: FuncInt, f2: FuncInt) extends IntExpressionResult {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

case class AssignInt(uInt: IntValue, expression: FuncInt) extends Assigner {
  def apply(): Unit = {
//    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
    uInt.value = expression()
  }
}


