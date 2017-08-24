// See LICENSE for license details.

package firrtl_interpreter.executable

trait Assigner {
  def apply(): Unit
}

trait IntAssigner {
  def apply(): Int
}

case class GetIntConstant(n: Int) {
  def apply(): Int = n
}

case class GetInt(state: ExecutableCircuit, index: Int) {
  val apply: () => Int = {
    if(true) nakedGetInt else verboseGetInt
  }

  def nakedGetInt(): Int = {
    state.ints(index)
  }
  def verboseGetInt(): Int = {
    println(s"getting int from index $index")
    state.ints(index)
  }
}

case class AddInts(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1() + f2()
}

case class SubInts(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1() - f2()
}

case class TailInts(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1()
}

case class MuxInts(condition: () => Int, trueClause: () => Int, falseClause: () => Int) {
  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
}

case class EqInts(f1: () => Int, f2: () => Int) {
  def apply(): Int = if(f1() == f2()) 1 else 0
}

case class GtInts(f1: () => Int, f2: () => Int) {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

class AddBigIntToInt(state: ExecutableCircuit, aIndex: Int, bIndex: Int) {
  def apply: BigInt = state.bigInts(aIndex) + state.ints(bIndex)
}

class AssignBigInt(state: ExecutableCircuit, index: Int, expression: => BigInt) {
  def apply(): Unit = state.bigInts(index) = expression
}

case class AssignInt(state: ExecutableCircuit, index: Int, expression: () => Int) extends Assigner {
  def apply(): Unit = {
//    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
    state.ints(index) = expression()
  }
}


