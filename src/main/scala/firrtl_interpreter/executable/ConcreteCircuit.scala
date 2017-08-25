// See LICENSE for license details.

package firrtl_interpreter.executable

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
  val values = new Array[IntValue](n)

  def getIndex(name: String): Int = names(name)
  def get(name: String): IntValue = values(names(name))
  def get(idx: Int): IntValue = values(idx)

  def header: String = {
    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }
  override def toString: String = {
    names.keys.toArray.sorted.map(get(_).value).map { b => f"$b%10d" }.mkString("")
  }
}

case class GetIntValuesConstant(n: Int) {
  def apply(): Int = n
}

case class GetIntValues(state: ConcreteCircuit, intValue: IntValue) {
  val apply: () => Int = {
    if(true) nakedGetIntValues else verboseGetIntValues
  }

  def nakedGetIntValues(): Int = {
    intValue.value
  }
  def verboseGetIntValues(): Int = {
    println(s"getting int from index ${intValue.value}")
    intValue.value
  }
}

case class AddIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1() + f2()
}

case class SubIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1() - f2()
}

case class TailIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = f1()
}

case class MuxIntValues(condition: () => Int, trueClause: () => Int, falseClause: () => Int) {
  def apply(): Int = if(condition() > 0) trueClause() else falseClause()
}

case class EqIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = if(f1() == f2()) 1 else 0
}

case class GtIntValues(f1: () => Int, f2: () => Int) {
  def apply(): Int = if(f1() > f2()) 1 else 0
}

case class AssignIntValues(state: ConcreteCircuit, index: IntValue, expression: () => Int) extends Assigner {
  def apply(): Unit = {
    //    println(s"assign index $index ${state.names.values.find(_.index == index).get.name} ${expression()}")
    index.value = expression()
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
      state.values(idx) = wire
    }

//    println(s"state 0 $state")

    val instructions = Array(
      AssignIntValues(state, state.get("t_13"),
        GtIntValues(
          GetIntValues(state, state.get("reg_x_out")).apply,
          GetIntValues(state, state.get("reg_y_out")).apply).apply _
      ),
      AssignIntValues(state, state.get("t_14"),
        SubIntValues(
          GetIntValues(state, state.get("reg_x_out")).apply,
          GetIntValues(state, state.get("reg_y_out")).apply).apply _
      ),
      AssignIntValues(state, state.get("t_15"),
        TailIntValues(
          GetIntValues(state, state.get("t_14")).apply,
          GetIntValuesConstant(1).apply _
        ).apply _
      ),
      AssignIntValues(state, state.get("t_17"),
        EqIntValues(
          GetIntValues(state, state.get("t_13")).apply,
          GetIntValuesConstant(0).apply _
        ).apply _
      ),
      AssignIntValues(state, state.get("t_18"),
        SubIntValues(
          GetIntValues(state, state.get("reg_y_out")).apply,
          GetIntValues(state, state.get("reg_x_out")).apply).apply _
      ),
      AssignIntValues(state, state.get("t_19"),
        TailIntValues(
          GetIntValues(state, state.get("t_18")).apply,
          GetIntValuesConstant(1).apply _
        ).apply _
      ),
      AssignIntValues(state, state.get("t_21"),
        EqIntValues(
          GetIntValues(state, state.get("reg_y_out")).apply,
          GetIntValuesConstant(0).apply _
        ).apply _
      ),
      AssignIntValues(state, state.get("gen_0"),
        MuxIntValues(
          GetIntValues(state, state.get("t_13")).apply,
          GetIntValues(state, state.get("t_15")).apply,
          GetIntValues(state, state.get("reg_x_out")).apply
        ).apply _
      ),
      AssignIntValues(state, state.get("gen_1"),
        MuxIntValues(
          GetIntValues(state, state.get("t_17")).apply,
          GetIntValues(state, state.get("t_19")).apply,
          GetIntValues(state, state.get("reg_y_out")).apply
        ).apply _
      ),
      AssignIntValues(state, state.get("io_z"),
        GetIntValues(state, state.get("reg_x_out")).apply
      ),
      AssignIntValues(state, state.get("io_v"),
        GetIntValues(state, state.get("t_21")).apply
      ),
      AssignIntValues(state, state.get("reg_x_in"),
        GetIntValues(state, state.get("t_21")).apply
      ),
      AssignIntValues(state, state.get("reg_x_in"),
        MuxIntValues(
          GetIntValues(state, state.get("io_e")).apply,
          GetIntValues(state, state.get("io_a")).apply,
          GetIntValues(state, state.get("gen_0")).apply
        ).apply _
      ),
      AssignIntValues(state, state.get("reg_y_in"),
        MuxIntValues(
          GetIntValues(state, state.get("io_e")).apply,
          GetIntValues(state, state.get("io_b")).apply,
          GetIntValues(state, state.get("gen_1")).apply
        ).apply _
      )
    )

    val regNextInstructions = Array(
      AssignIntValues(state, state.get("reg_x_out"),
        GetIntValues(state, state.get("reg_x_in")).apply
      ),
      AssignIntValues(state, state.get("reg_y_out"),
        GetIntValues(state, state.get("reg_y_in")).apply
      )
    )

    def pokeIdx(idx: Int, value: Int): Unit = {
      state.get(idx).value = value
    }
    def poke(name: String, value: Int): Unit = {
      pokeIdx(state.getIndex(name), value)
    }
    def peekIdx(idx: Int): Int = {
      state.get(idx).value
    }
    def peek(name: String): Int = {
      peekIdx(state.getIndex(name))
    }
    def expect(name: String, value: Int, msg: => String) = {
      assert(peek(name) == value,
        s"${peek(name)} did not equal $value, $msg")
    }

    var cycle = 0
    def step(): Unit = {
      var i = 0
      while (i < regNextInstructions.length) {
        regNextInstructions(i)()
        i += 1
      }
      i = 0
      while (i < instructions.length) {
        instructions(i)()
        i += 1
      }
      cycle += 1
    }

    def show(): Unit = {
      println(f"state $cycle%6d $state")
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

      expect("io_z", z, s"$x, $y")
      //      show()

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
      for {x <- 1 to 1000
           y <- 1 to 1000
      } yield (x, y, computeGcd(x, y)._1)

    runOnce(values)
    runOnce(values)
    runOnce(values)
    // ExecutableCircuit.runOnce(values)
    // ExecutableCircuit.runOnce(values)
    // ExecutableCircuit.runOnce(values)
  }
}
