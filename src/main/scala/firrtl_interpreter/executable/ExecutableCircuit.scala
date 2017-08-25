// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

class ExecutableCircuit {
  val names: mutable.HashMap[String, ExecutableValue] = new mutable.HashMap[String, ExecutableValue]

  def header: String = {
    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }

  def addWire(wireValue: ExecutableValue): Unit = {
    names(wireValue.name) = wireValue
  }

  def apply(name: String): ExecutableValue = {
    names(name)
  }

  def getUInt(name: String): UInt = {
    names(name).asInstanceOf[UInt]
  }

  override def toString: String = {
    names.keys.toArray.sorted.map { key => f"${names(key).asBigInt}%10d" }.mkString("")
  }
}

//noinspection ScalaStyle,ScalaUnusedSymbol
object ExecutableCircuit {
  def apply(nameMap: Map[String, UInt]): ExecutableCircuit = {
    val (bigWireCount, intWireCount) = nameMap.values.foldLeft((0, 0)) { case ((aCount, bCount), wireValue) =>
      if (wireValue.bitSize > 32) (aCount + 1, bCount) else (aCount, bCount + 1)
    }
    new ExecutableCircuit
  }

  def runOnce(values: Seq[(Int, Int, Int)]): Unit = {
    var nextWire = -1

    def newNextWire() = {
      nextWire += 1; nextWire
    }

    val wires = Seq(
      UInt("io_a", 32),
      UInt("io_b", 32),
      UInt("io_e", 32),
      UInt("io_z", 32),
      UInt("io_v", 32),
      UInt("reg_x_in", 32),
      UInt("reg_x_out", 32),
      UInt("reg_y_in", 32),
      UInt("reg_y_out", 32),
      UInt("t_13", 32),
      UInt("t_14", 32),
      UInt("t_15", 32),
      UInt("t_16", 32),
      UInt("t_17", 32),
      UInt("t_18", 32),
      UInt("t_19", 32),
      UInt("t_20", 32),
      UInt("t_21", 32),
      UInt("gen_0", 32),
      UInt("gen_1", 32)
    )
    val state = new ExecutableCircuit

    wires.foreach { wv => state.addWire(wv) }

    //    println(s"state 0 $state")

    val instructions = Array[Assigner](
      AssignInt(state, state.getUInt("t_13"),
        GtInts(
          GetInt(state, state.getUInt("reg_x_out")).apply,
          GetInt(state, state.getUInt("reg_y_out")).apply).apply _
      ),
      AssignInt(state, state.getUInt("t_14"),
        SubInts(
          GetInt(state, state.getUInt("reg_x_out")).apply,
          GetInt(state, state.getUInt("reg_y_out")).apply).apply _
      ),
      AssignInt(state, state.getUInt("t_15"),
        TailInts(
          GetInt(state, state.getUInt("t_14")).apply,
          GetIntConstant(1).apply _
        ).apply _
      ),
      AssignInt(state, state.getUInt("t_17"),
        EqInts(
          GetInt(state, state.getUInt("t_13")).apply,
          GetIntConstant(0).apply _
        ).apply _
      ),
      AssignInt(state, state.getUInt("t_18"),
        SubInts(
          GetInt(state, state.getUInt("reg_y_out")).apply,
          GetInt(state, state.getUInt("reg_x_out")).apply).apply _
      ),
      AssignInt(state, state.getUInt("t_19"),
        TailInts(
          GetInt(state, state.getUInt("t_18")).apply,
          GetIntConstant(1).apply _
        ).apply _
      ),
      AssignInt(state, state.getUInt("t_21"),
        EqInts(
          GetInt(state, state.getUInt("reg_y_out")).apply,
          GetIntConstant(0).apply _
        ).apply _
      ),
      AssignInt(state, state.getUInt("gen_0"),
        MuxInts(
          GetInt(state, state.getUInt("t_13")).apply,
          GetInt(state, state.getUInt("t_15")).apply,
          GetInt(state, state.getUInt("reg_x_out")).apply
        ).apply _
      ),
      AssignInt(state, state.getUInt("gen_1"),
        MuxInts(
          GetInt(state, state.getUInt("t_17")).apply,
          GetInt(state, state.getUInt("t_19")).apply,
          GetInt(state, state.getUInt("reg_y_out")).apply
        ).apply _
      ),
      AssignInt(state, state.getUInt("io_z"), GetInt(state, state.getUInt("reg_x_out")).apply),
      AssignInt(state, state.getUInt("io_v"), GetInt(state, state.getUInt("t_21")).apply),
      AssignInt(state, state.getUInt("reg_x_in"), GetInt(state, state.getUInt("t_21")).apply),
      AssignInt(state, state.getUInt("reg_x_in"),
        MuxInts(
          GetInt(state, state.getUInt("io_e")).apply,
          GetInt(state, state.getUInt("io_a")).apply,
          GetInt(state, state.getUInt("gen_0")).apply
        ).apply _
      ),
      AssignInt(state, state.getUInt("reg_y_in"),
        MuxInts(
          GetInt(state, state.getUInt("io_e")).apply,
          GetInt(state, state.getUInt("io_b")).apply,
          GetInt(state, state.getUInt("gen_1")).apply
        ).apply _
      )
    )

    val regNextInstructions = Array(
      AssignInt(state, state.getUInt("reg_x_out"), GetInt(state, state.getUInt("reg_x_in")).apply),
      AssignInt(state, state.getUInt("reg_y_out"), GetInt(state, state.getUInt("reg_y_in")).apply)
    )

    def poke(name: String, value: Int): Unit = {
      state.getUInt(name).value = value
    }

    def peek(name: String): Int = {
      state.getUInt(name).value
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

    def computeGcd(a: Int, b: Int): (Int, Int) = {
      var x = a
      var y = b
      var depth = 1
      while (y > 0) {
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

    val startTime = System.nanoTime()

    values.foreach { case (x, y, z) =>

      poke("io_a", x)
      poke("io_b", y)
      poke("io_e", 1)

      step()

      poke("io_e", 0)
      step()

      while (peek("io_v") != 1) {
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
  }
}
