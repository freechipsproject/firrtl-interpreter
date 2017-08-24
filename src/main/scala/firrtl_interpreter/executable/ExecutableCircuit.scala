// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

class ExecutableCircuit {
  var bigInts: Array[BigInt] = Array[BigInt](0)
  var ints: Array[Int] = Array[Int](0)
  val names: mutable.HashMap[String, WireValue] = new mutable.HashMap[String, WireValue]

  def getIndex(name: String): Int = {
    names(name).index
  }

  def header: String = {
    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }

  def build(): Unit = {
    val (bigIntsCount, intsCount) = names.values.foldLeft((0, 0)) { case ((bc, ic), wireValue) =>
      if(wireValue.isBig) (bc + 1, ic) else (bc, ic + 1)
    }
    bigInts = Array.fill(bigIntsCount)(BigInt(0))
    ints =  Array.fill(intsCount)(0)
  }

  def addWire(wireValue: WireValue): Unit = {
    names(wireValue.name) = wireValue
  }

  override def toString: String = {
    names.keys.toArray.sorted.map(names(_)).map { wire =>
      f"${if(wire.isBig) bigInts(wire.index) else BigInt(ints(wire.index))}%10d"
    }.mkString("")
  }
}

//noinspection ScalaStyle,ScalaUnusedSymbol
object ExecutableCircuit {
  def apply(nameMap: Map[String, WireValue]): ExecutableCircuit = {
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
    val state = new ExecutableCircuit

    wires.foreach { wv => state.addWire(wv) }
    state.build()

    //    println(s"state 0 $state")

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
      val index = state.getIndex(name)
      state.ints(index) = value
    }

    def peek(name: String): Int = {
      state.ints(state.getIndex(name))
    }

    def expect(name: String, value: Int, msg: => String) = {
      assert(state.ints(state.getIndex(name)) == value,
        s"${state.ints(state.getIndex(name))} did not equal $value, $msg")
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
