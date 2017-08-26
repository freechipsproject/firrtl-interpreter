// See LICENSE for license details.

package firrtl_interpreter.executable

import gcd._

import scala.collection.mutable

trait BaseValue {
  // def name: String
  def size: Int
}

case class IntValue(size: Int) extends BaseValue {
  var value: Int = 0
  def apply(): Int = value
}

case class BigValue(name: String, isSigned: Boolean, size: Int) extends BaseValue {
  var value: BigInt = 0
}

class ConcreteCircuit(n: Int) {
  val names: mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]
  val widths = new Array[Int](n)
  val values = new Array[Int](n)

  def getIndex(name: String): Int = names(name)
  def get(name: String): Int = values(names(name))
  def get(idx: Int): Int = values(idx)

  def header: String = {
    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }
  override def toString: String = {
    names.keys.toArray.sorted.map(get(_)).map { b => f"$b%10d" }.mkString("")
  }
}

object OperationImplementations {
  def main(arg: Array[String]): Unit = {
    val a = new GCDImpl(Array(1,2,3,4,5))
    println(a.values)
  }
  def getIntConst(n: Int): String = {
    if (n < 4) {
      s"iconst_$n"
    } else {
      s"ldc            $n"
    }
  }
  def outputJasmin(node: Node): String = node match {
    case GetIntValuesConstantNode(n) =>
      s"""
      ; GetIntValuesConstantNode($n)
      ${getIntConst(n)}
      """
    case GetIntValuesNode(idx) =>
      s"""
      ; GetIntValuesNode($idx)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ${getIntConst(idx)}
      iaload
      """
    case AddIntValuesNode(n1, n2) =>
      s"""
      ; AddIntValuesNode($n1, $n2)
      ${outputJasmin(n1)}
      ${outputJasmin(n2)}
      ; finish AddIntValuesNode($n1, $n2)
      invokestatic   firrtl_interpreter/executable/OperationImplementations/addInt(II)I
      """
    case SubIntValuesNode(n1, n2) =>
      s"""
      ; SubIntValuesNode($n1, $n2)
      ${outputJasmin(n1)}
      ${outputJasmin(n2)}
      ; finish SubIntValuesNode($n1, $n2)
      invokestatic   firrtl_interpreter/executable/OperationImplementations/subInt(II)I
      """
    case TailIntValuesNode(n1, n2) =>
      s"""
      ; TailIntValuesNode($n1, $n2)
      ${outputJasmin(n1)}
      ${outputJasmin(n2)}
      ; finish TailIntValuesNode($n1, $n2)
      invokestatic   firrtl_interpreter/executable/OperationImplementations/tailInt(II)I
      """
    case MuxIntValuesNode(n1, n2, n3) =>
      s"""
      ; MuxIntValuesNode($n1, $n2, $n3)
      ${outputJasmin(n1)}
      ${outputJasmin(n2)}
      ${outputJasmin(n3)}
      ; finish MuxIntValuesNode($n1, $n2, $n3)
      invokestatic   firrtl_interpreter/executable/OperationImplementations/muxInt(III)I
      """
    case EqIntValuesNode(n1, n2) =>
      s"""
      ; EqIntValuesNode($n1, $n2)
      ${outputJasmin(n1)}
      ${outputJasmin(n2)}
      ; finish EqIntValuesNode($n1, $n2)
      invokestatic   firrtl_interpreter/executable/OperationImplementations/eqInt(II)I
      """
    case GtIntValuesNode(n1, n2) =>
      s"""
      ; GtIntValuesNode($n1, $n2)
      ${outputJasmin(n1)}
      ${outputJasmin(n2)}
      ; finish GtIntValuesNode($n1, $n2)
      invokestatic   firrtl_interpreter/executable/OperationImplementations/gtInt(II)I
      """
    case AssignIntValuesNode(index, expr) =>
      s"""
      ; AssignIntValuesNode($index, $expr)
      aload_0
      getfield       gcd/GCDImpl/values [I
      ${getIntConst(index)}
      ${outputJasmin(expr)}
      ; finish AssignIntValuesNode($index, $expr)
      iastore
      """
  }
  def outputJasmin(ast: Seq[Node]): String = {
    ast.foldLeft(""){case (str, node) => str + outputJasmin(node) } + "\n\nreturn"
  }
  def addInt(in1: Int, in2: Int): Int = {
    in1 + in2
  }
  def subInt(in1: Int, in2: Int): Int = {
    in1 - in2
  }
  def tailInt(in1: Int, in2: Int): Int = {
    in1
  }
  def muxInt(sel: Int, trueBranch: Int, falseBranch: Int): Int = {
    if (sel != 0) trueBranch else falseBranch
  }
  def eqInt(in1: Int, in2: Int): Int = {
    if (in1 == in2) 1 else 0
  }
  def gtInt(in1: Int, in2: Int): Int = {
    if (in1 > in2) 1 else 0
  }
}

sealed trait Node
case class GetIntValuesConstantNode(n: Int) extends Node
class GetIntValuesConstant(n: Int) {
  def apply(): Int = n
}
object GetIntValuesConstant {
  def apply(n: Int) = new GetIntValuesConstant(n)
}

case class GetIntValuesNode(idx: Int) extends Node
case class GetIntValues(state: ConcreteCircuit, idx: Int) {
  val apply: () => Int = {
    if(true) nakedGetIntValues else verboseGetIntValues
  }

  def nakedGetIntValues(): Int = {
    state.values(idx)
  }
  def verboseGetIntValues(): Int = {
    println(s"getting int from index $idx = ${state.values(idx)}")
    state.values(idx)
  }
}

case class AddIntValuesNode(f1: Node, f2: Node) extends Node
class AddIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1() + f2()
}
object AddIntValues {
  def apply(f1: () => Int, f2: () => Int) = new AddIntValues(f1, f2)
}

case class SubIntValuesNode(f1: Node, f2: Node) extends Node
class SubIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1() - f2()
}
object SubIntValues {
  def apply(f1: () => Int, f2: () => Int) = new SubIntValues(f1, f2)
}

case class TailIntValuesNode(f1: Node, f2: Node) extends Node
class TailIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1()
}
object TailIntValues {
  def apply(f1: () => Int, f2: () => Int) = new TailIntValues(f1, f2)
}

case class MuxIntValuesNode(cond: Node, trueClause: Node, falseClause: Node) extends Node
class MuxIntValues(condition: () => Int, trueClause: () => Int, falseClause: () => Int) {
  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
}
object MuxIntValues {
  def apply(condition: () => Int, trueClause: () => Int, falseClause: () => Int) = new MuxIntValues(condition, trueClause, falseClause)
}

case class EqIntValuesNode(f1: Node, f2: Node) extends Node
class EqIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = if(f1() == f2()) 1 else 0
}
object EqIntValues {
  def apply(f1: () => Int, f2: () => Int) = new EqIntValues(f1, f2)
}

case class GtIntValuesNode(f1: Node, f2: Node) extends Node
class GtIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = if(f1() > f2()) 1 else 0
}
object GtIntValues {
  def apply(f1: () => Int, f2: () => Int) = new GtIntValues(f1, f2)
}

case class AssignIntValuesNode(index: Int, expression: Node) extends Node
case class AssignIntValues(state: ConcreteCircuit, index: Int, expression: () => Int) extends Assigner {
  def apply(): Unit = {
    //    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
    state.values(index) = expression()
  }
}

//noinspection ScalaStyle,ScalaUnusedSymbol
object ConcreteCircuit {
  def apply(nameMap: Map[String, UInt]): ConcreteCircuit = {
    val (bigWireCount, intWireCount) = nameMap.values.foldLeft((0, 0)) { case ((aCount, bCount), wireValue) =>
      if(wireValue.bitSize > 32) (aCount + 1, bCount) else (aCount, bCount + 1)
    }
    new ConcreteCircuit(nameMap.size)
  }

  def runOnce(values: Seq[(Int, Int, Int)]): Unit ={
    var nextWire = -1
    def newNextWire() = { nextWire += 1; nextWire }

    val wires = Seq(
      "io_a"      -> IntValue(32),
      "io_b"      -> IntValue(32),
      "io_e"      -> IntValue(32),
      "io_z"      -> IntValue(32),
      "io_v"      -> IntValue(32),
      "reg_x_in"  -> IntValue(32),
      "reg_x_out" -> IntValue(32),
      "reg_y_in"  -> IntValue(32),
      "reg_y_out" -> IntValue(32),
      "t_13"      -> IntValue(32),
      "t_14"      -> IntValue(32),
      "t_15"      -> IntValue(32),
      "t_16"      -> IntValue(32),
      "t_17"      -> IntValue(32),
      "t_18"      -> IntValue(32),
      "t_19"      -> IntValue(32),
      "t_20"      -> IntValue(32),
      "t_21"      -> IntValue(32),
      "gen_0"     -> IntValue(32),
      "gen_1"     -> IntValue(32)
    )
    val state = new ConcreteCircuit(wires.length)
    wires.zipWithIndex.foreach { case ((name, wire), idx) =>
      state.names(name) = idx
      state.values(idx) = 0
      state.widths(idx) = wire.size
    }

//    println(s"state 0 $state")

    val destinations = Array(
      state.getIndex("t_13"),
      state.getIndex("t_14"),
      state.getIndex("t_15"),
      state.getIndex("t_17"),
      state.getIndex("t_18"),
      state.getIndex("t_19"),
      state.getIndex("t_21"),
      state.getIndex("gen_0"),
      state.getIndex("gen_1"),
      state.getIndex("io_z"),
      state.getIndex("io_v"),
      state.getIndex("reg_x_in"),
      state.getIndex("reg_x_in"),
      state.getIndex("reg_y_in"),
      state.getIndex("t_19"),
      state.getIndex("t_21"),
      state.getIndex("gen_0"),
      state.getIndex("gen_1"),
      state.getIndex("io_z"),
      state.getIndex("io_v"),
      state.getIndex("reg_x_in"),
      state.getIndex("reg_x_in"),
      state.getIndex("reg_y_in")
    )
    val instructions = Array(
      GtIntValues(
        GetIntValues(state, state.getIndex("reg_x_out")).apply,
        GetIntValues(state, state.getIndex("reg_y_out")).apply).apply _,
      SubIntValues(
        GetIntValues(state, state.getIndex("reg_x_out")).apply,
        GetIntValues(state, state.getIndex("reg_y_out")).apply).apply _,
      TailIntValues(
        GetIntValues(state, state.getIndex("t_14")).apply,
        GetIntValuesConstant(1).apply _
      ).apply _,
      EqIntValues(
        GetIntValues(state, state.getIndex("t_13")).apply,
        GetIntValuesConstant(0).apply _
      ).apply _,
      SubIntValues(
        GetIntValues(state, state.getIndex("reg_y_out")).apply,
        GetIntValues(state, state.getIndex("reg_x_out")).apply).apply _,
      TailIntValues(
        GetIntValues(state, state.getIndex("t_18")).apply,
        GetIntValuesConstant(1).apply _
      ).apply _ ,
      EqIntValues(
        GetIntValues(state, state.getIndex("reg_y_out")).apply,
        GetIntValuesConstant(0).apply _
      ).apply _ ,
      MuxIntValues(
        GetIntValues(state, state.getIndex("t_13")).apply,
        GetIntValues(state, state.getIndex("t_15")).apply,
        GetIntValues(state, state.getIndex("reg_x_out")).apply
      ).apply _ ,
      MuxIntValues(
        GetIntValues(state, state.getIndex("t_17")).apply,
        GetIntValues(state, state.getIndex("t_19")).apply,
        GetIntValues(state, state.getIndex("reg_y_out")).apply
      ).apply _ ,
      GetIntValues(state, state.getIndex("reg_x_out")).apply,
      GetIntValues(state, state.getIndex("t_21")).apply,
      GetIntValues(state, state.getIndex("t_21")).apply,
      MuxIntValues(
        GetIntValues(state, state.getIndex("io_e")).apply,
        GetIntValues(state, state.getIndex("io_a")).apply,
        GetIntValues(state, state.getIndex("gen_0")).apply
      ).apply _ ,
      MuxIntValues(
        GetIntValues(state, state.getIndex("io_e")).apply,
        GetIntValues(state, state.getIndex("io_b")).apply,
        GetIntValues(state, state.getIndex("gen_1")).apply
      ).apply _
    )

    val regNextDestinations = Array(
      state.getIndex("reg_x_out"),
      state.getIndex("reg_y_out")
    )
    val regNextInstructions = Array(
      GetIntValues(state, state.getIndex("reg_x_in")).apply,
      GetIntValues(state, state.getIndex("reg_y_in")).apply
    )
    val instructionList = Seq[Node](
      AssignIntValuesNode(
        state.getIndex("reg_x_out"),
        GetIntValuesNode(state.getIndex("reg_x_in"))),
      AssignIntValuesNode(
        state.getIndex("reg_y_out"),
        GetIntValuesNode(state.getIndex("reg_y_in"))),
      AssignIntValuesNode(
        state.getIndex("t_13"),
        GtIntValuesNode(
          GetIntValuesNode(state.getIndex("reg_x_out")),
          GetIntValuesNode(state.getIndex("reg_y_out")))),
      AssignIntValuesNode(
        state.getIndex("t_14"),
        SubIntValuesNode(
          GetIntValuesNode(state.getIndex("reg_x_out")),
          GetIntValuesNode(state.getIndex("reg_y_out")))),
      AssignIntValuesNode(
        state.getIndex("t_15"),
        TailIntValuesNode(
          GetIntValuesNode(state.getIndex("t_14")),
          GetIntValuesConstantNode(1))),
      AssignIntValuesNode(
        state.getIndex("t_17"),
        EqIntValuesNode(
          GetIntValuesNode(state.getIndex("t_13")),
          GetIntValuesConstantNode(0))),
      AssignIntValuesNode(
        state.getIndex("t_18"),
        SubIntValuesNode(
          GetIntValuesNode(state.getIndex("reg_y_out")),
          GetIntValuesNode(state.getIndex("reg_x_out")))),
      AssignIntValuesNode(
        state.getIndex("t_19"),
        TailIntValuesNode(
          GetIntValuesNode(state.getIndex("t_18")),
          GetIntValuesConstantNode(1))),
      AssignIntValuesNode(
        state.getIndex("t_21"),
        EqIntValuesNode(
          GetIntValuesNode(state.getIndex("reg_y_out")),
          GetIntValuesConstantNode(0))),
      AssignIntValuesNode(
        state.getIndex("gen_0"),
        MuxIntValuesNode(
          GetIntValuesNode(state.getIndex("t_13")),
          GetIntValuesNode(state.getIndex("t_15")),
          GetIntValuesNode(state.getIndex("reg_x_out")))),
      AssignIntValuesNode(
        state.getIndex("gen_1"),
        MuxIntValuesNode(
          GetIntValuesNode(state.getIndex("t_17")),
          GetIntValuesNode(state.getIndex("t_19")),
          GetIntValuesNode(state.getIndex("reg_y_out")))),
      AssignIntValuesNode(
        state.getIndex("io_z"),
        GetIntValuesNode(state.getIndex("reg_x_out"))),
      AssignIntValuesNode(
        state.getIndex("io_v"),
        GetIntValuesNode(state.getIndex("t_21"))),
      AssignIntValuesNode(
        state.getIndex("reg_x_in"),
        GetIntValuesNode(state.getIndex("t_21"))),
      AssignIntValuesNode(
        state.getIndex("reg_x_in"),
        MuxIntValuesNode(
          GetIntValuesNode(state.getIndex("io_e")),
          GetIntValuesNode(state.getIndex("io_a")),
          GetIntValuesNode(state.getIndex("gen_0")))),
      AssignIntValuesNode(
        state.getIndex("reg_y_in"),
        MuxIntValuesNode(
          GetIntValuesNode(state.getIndex("io_e")),
          GetIntValuesNode(state.getIndex("io_b")),
          GetIntValuesNode(state.getIndex("gen_1"))))
    )
    // println(OperationImplementations.outputJasmin(instructionList))

    val imp = new GCDImpl(new Array[Int](20))

    def pokeIdx(idx: Int, value: Int): Unit = {
      imp.values(idx) = value
      //state.values(idx) = value
    }
    def poke(name: String, value: Int): Unit = {
      pokeIdx(state.getIndex(name), value)
    }
    def peekIdx(idx: Int): Int = {
      imp.values(idx)
      // state.values(idx)
    }
    def peek(name: String): Int = {
      peekIdx(state.getIndex(name))
    }
    def expectIdx(idx: Int, value: Int, msg: => String) = {
      assert(peekIdx(idx) == value,
        s"${peekIdx(idx)} did not equal $value, $msg")
    }
    def expect(name: String, value: Int, msg: => String) = {
      expectIdx(state.getIndex(name), value, msg)
    }

    var cycle = 0
    def step(): Unit = {
      imp.step()
      // var i = 0
      // while (i < regNextInstructions.length) {
      //   state.values(regNextDestinations(i)) = regNextInstructions(i)()
      //   i += 1
      // }
      // i = 0
      // while (i < instructions.length) {
      //   // instructions(i)()
      //   state.values(destinations(i)) = instructions(i)()
      //   i += 1
      // }
      cycle += 1
    }

    def show(): Unit = {
      println(s"cycle $cycle")
      println(s"state ${imp.values.map(_.toString).reduce(_+" "+_)}")
      println()

      // println(f"state $cycle%6d $state")
    }

//    println(f"state ${""}%6.6s  ${state.header}")

    val io_a = state.getIndex("io_a")
    val io_b = state.getIndex("io_b")
    val io_e = state.getIndex("io_e")
    val io_v = state.getIndex("io_v")
    val io_z = state.getIndex("io_z")

    val startTime = System.nanoTime()

    values.foreach { case (x, y, z) =>


      pokeIdx(io_a, x)
      pokeIdx(io_b, y)
      pokeIdx(io_e, 1)

      step()

      pokeIdx(io_e, 0)
      step()

      while(peekIdx(io_v) != 1) {
        step()
      }

      expectIdx(io_z, z, s"$x, $y")
      // show()

    }

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println(
      f"processed $cycle cycles $elapsedSeconds%.6f seconds ${cycle.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
  }

  def main(args: Array[String]): Unit = {
    def computeGcd(a: Int, b: Int): (Int, Int) = {
      var x = a
      var y = b
      var depth = 1
      while(y > 0 ) {
        if (x > y) {
          x -= y
        }
        else {
          y -= x
        }
        depth += 1
      }
      (x, depth)
    }

    val values =
      for {x <- 1 to 4000
           y <- 1 to 4000
      } yield (x, y, computeGcd(x, y)._1)

    runOnce(values)
    runOnce(values)
    runOnce(values)
    // ExecutableCircuit.runOnce(values)
    // ExecutableCircuit.runOnce(values)
    // ExecutableCircuit.runOnce(values)
  }
}
