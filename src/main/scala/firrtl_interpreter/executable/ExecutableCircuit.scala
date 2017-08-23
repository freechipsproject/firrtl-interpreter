// See LICENSE for license details.

package firrtl_interpreter.executable

class ExecutableCircuit(numberOfBigInts: Int, numberOfInts: Int, nameMap: Map[String, WireValue]) {
  val bigInts: Array[BigInt] = Array.fill(numberOfBigInts){ BigInt(0) }
  val ints: Array[Int] = Array.fill(numberOfInts){ 0 }
  val names: Map[String, WireValue] = nameMap

  def getIndex(name: String): Int = {
    names(name).index
  }

  def header: String = {
    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }
  override def toString: String = {
    names.keys.toArray.sorted.map(names(_)).map { wire =>
      f"${if(wire.bitSize > 32) bigInts(wire.index) else BigInt(ints(wire.index))}%10d"
    }.mkString("")
  }
}

//noinspection ScalaStyle,ScalaUnusedSymbol
object ExecutableCircuit {
  def apply(nameMap: Map[String, WireValue]): ExecutableCircuit = {
    val (bigWireCount, intWireCount) = nameMap.values.foldLeft((0, 0)) { case ((aCount, bCount), wireValue) =>
      if(wireValue.bitSize > 32) (aCount + 1, bCount) else (aCount, bCount + 1)
    }
    new ExecutableCircuit(bigWireCount, intWireCount, nameMap)
  }

  def main(args: Array[String]): Unit = {
    var nextWire = -1
    def newNextWire() = { nextWire += 1; nextWire }

    val wires = Seq(
      WireValue("io_a", isSigned = false, 32, newNextWire()),
      WireValue("io_b", isSigned = false, 32, newNextWire()),
      WireValue("io_e", isSigned = false, 32, newNextWire()),
      WireValue("io_z", isSigned = false, 32, newNextWire()),
      WireValue("io_v", isSigned = false, 32, newNextWire()),
      WireValue("reg_x_in", isSigned = false, 32, newNextWire()),
      WireValue("reg_x_out", isSigned = false, 32, newNextWire()),
      WireValue("reg_y_in", isSigned = false, 32, newNextWire()),
      WireValue("reg_y_out", isSigned = false, 32, newNextWire()),
      WireValue("t_13", isSigned = false, 32, newNextWire()),
      WireValue("t_14", isSigned = false, 32, newNextWire()),
      WireValue("t_15", isSigned = false, 32, newNextWire()),
      WireValue("t_16", isSigned = false, 32, newNextWire()),
      WireValue("t_17", isSigned = false, 32, newNextWire()),
      WireValue("t_18", isSigned = false, 32, newNextWire()),
      WireValue("t_19", isSigned = false, 32, newNextWire()),
      WireValue("t_20", isSigned = false, 32, newNextWire()),
      WireValue("t_21", isSigned = false, 32, newNextWire()),
      WireValue("gen_0", isSigned = false, 32, newNextWire()),
      WireValue("gen_1", isSigned = false, 32, newNextWire())
    )
    val state = ExecutableCircuit(wires.map { wv => (wv.name, wv) }.toMap)

    println(s"state 0 $state")

    val instructions = Seq(
      AssignInt(state, state.getIndex("t_13"),
        GtInts(
          GetInt(state, state.getIndex("reg_x_out")).apply,
          GetInt(state, state.getIndex("reg_y_out")).apply).apply _
      ),
      AssignInt(state, state.getIndex("t_14"),
        SubInts(
          GetInt(state, state.getIndex("reg_x_out")).apply,
          GetInt(state, state.getIndex("reg_y_out")).apply).apply _
      ),
      AssignInt(state, state.getIndex("t_15"),
        TailInts(
          GetInt(state, state.getIndex("t_14")).apply,
          GetIntConstant(1).apply _
        ).apply _
      ),
      AssignInt(state, state.getIndex("t_17"),
        EqInts(
          GetInt(state, state.getIndex("t_13")).apply,
          GetIntConstant(0).apply _
        ).apply _
      ),
      AssignInt(state, state.getIndex("t_18"),
        SubInts(
          GetInt(state, state.getIndex("reg_y_out")).apply,
          GetInt(state, state.getIndex("reg_x_out")).apply).apply _
      ),
      AssignInt(state, state.getIndex("t_19"),
        TailInts(
          GetInt(state, state.getIndex("t_18")).apply,
          GetIntConstant(1).apply _
        ).apply _
      ),
      AssignInt(state, state.getIndex("t_21"),
        EqInts(
          GetInt(state, state.getIndex("reg_y_out")).apply,
          GetIntConstant(0).apply _
        ).apply _
      ),
      AssignInt(state, state.getIndex("gen_0"),
        MuxInts(
          GetInt(state, state.getIndex("t_13")).apply,
          GetInt(state, state.getIndex("t_15")).apply,
          GetInt(state, state.getIndex("reg_x_out")).apply
        ).apply _
      ),
      AssignInt(state, state.getIndex("gen_1"),
        MuxInts(
          GetInt(state, state.getIndex("t_17")).apply,
          GetInt(state, state.getIndex("t_19")).apply,
          GetInt(state, state.getIndex("reg_y_out")).apply
        ).apply _
      ),
      AssignInt(state, state.getIndex("io_z"),
        GetInt(state, state.getIndex("reg_x_out")).apply
      ),
      AssignInt(state, state.getIndex("io_v"),
        GetInt(state, state.getIndex("t_21")).apply
      ),
      AssignInt(state, state.getIndex("reg_x_in"),
        GetInt(state, state.getIndex("t_21")).apply
      ),
      AssignInt(state, state.getIndex("reg_x_in"),
        MuxInts(
          GetInt(state, state.getIndex("io_e")).apply,
          GetInt(state, state.getIndex("io_a")).apply,
          GetInt(state, state.getIndex("gen_0")).apply
        ).apply _
      ),
      AssignInt(state, state.getIndex("reg_y_in"),
        MuxInts(
          GetInt(state, state.getIndex("io_e")).apply,
          GetInt(state, state.getIndex("io_b")).apply,
          GetInt(state, state.getIndex("gen_1")).apply
        ).apply _
      )
    )

    val regNextInstructions = Seq(
      AssignInt(state, state.getIndex("reg_x_out"),
        GetInt(state, state.getIndex("reg_x_in")).apply
      ),
      AssignInt(state, state.getIndex("reg_y_out"),
        GetInt(state, state.getIndex("reg_y_in")).apply
      )
    )

    def poke(name: String, value: Int): Unit = {
      state.ints(state.getIndex(name)) = value
    }
    def peek(name: String): Int = {
      state.ints(state.getIndex(name))
    }
    var cycle = 0
    def step(): Unit = {
      regNextInstructions.foreach { inst => inst() }
      instructions.foreach { inst => inst() }
      cycle += 1
    }

    def show(): Unit = {
      println(f"state $cycle%6d $state")
    }

    println(f"state ${""}%6.6s  ${state.header}")

    val startTime = System.nanoTime()

    for {x <- 1 to 1000
         y <- 1 to 1000
    } {

      poke("io_a", x)
      poke("io_b", y)
      poke("io_e", 1)

      step()

      poke("io_e", 0)
      step()

      while(peek("io_v") != 1) {
        step()
      }
//      show()
    }

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println(f"processed $cycle cycles $elapsedSeconds%.6f seconds")
  }
}
