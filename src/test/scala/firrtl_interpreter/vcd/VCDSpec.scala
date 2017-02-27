// See LICENSE for license details.

package firrtl_interpreter.vcd

import firrtl_interpreter.{InterpreterOptionsManager, InterpretiveTester}
import firrtl.CommonOptions
import firrtl.util.BackendCompilationUtilities
import java.io.File
import org.scalatest.{Matchers, FlatSpec}

// scalastyle:off magic.number
class VCDSpec extends FlatSpec with Matchers with BackendCompilationUtilities {
  private def getVcd = {
    VCD("test_circuit")
  }

  behavior of "vcd"

  it should "be able to generate unique ids " in {
    val vcd = getVcd

    val ids = new collection.mutable.HashSet[String]
    for (i <- 0 to 1000) {
      val id = vcd.getIdString(i)

      ids.contains(id) should be(false)
      ids += id

      id.forall { c => c.toInt >= 33 && c.toInt <= 126 } should be(true)
    }
  }

  it should "allow add wires" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    vcd.wires.contains("bob") should be(true)
    vcd.wires.contains("carol") should be(true)
    vcd.wires.contains("ted") should be(true)

    vcd.wires.contains("alice") should be(false)
  }

  it should "ignore calls to wire changed when value has not changed" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    // time starts at -1 to support initialized values
    vcd.incrementTime()
    for(i <- 0 to 10) {
      vcd.wireChanged("bob", i)
      vcd.wireChanged("carol", i / 2)
      vcd.wireChanged("ted", i / 4)
      vcd.incrementTime()
    }

    vcd.valuesAtTime(1).size should be (3)
    vcd.valuesAtTime(2).size should be (1)
    vcd.valuesAtTime(3).size should be (2)
    vcd.valuesAtTime(4).size should be (1)
    vcd.valuesAtTime(5).size should be (3)
    vcd.valuesAtTime(6).size should be (1)

    println(vcd.serialize)
  }

  behavior of "VCD reader"

  it should "be able to read a file" in {
    val tempFile = File.createTempFile("GCD", ".vcd")
    tempFile.deleteOnExit()
    copyResourceToFile("/GCD.vcd", tempFile)
    val vcdFile = VCD.read(tempFile.getCanonicalPath)

    vcdFile.date should be ("2016-10-13T16:31+0000")
  }

  behavior of "vcd log containing negative numbers"

  it should "work correctly and be runnable from vcd output file" in  {

    val input =
      """
        |circuit Adder :
        |  module Adder :
        |    input clock : Clock
        |    input a : SInt<8>
        |    input b : SInt<8>
        |    output c : SInt<10>
        |
        |    c <= add(a, b)
      """.stripMargin

    val manager = new InterpreterOptionsManager {
      interpreterOptions = interpreterOptions.copy(writeVCD = true)
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }
    val interpreter = new InterpretiveTester(input, manager)
    interpreter.poke("a", -1)
    interpreter.peek("a") should be (BigInt(-1))
    interpreter.poke("b", -7)
    interpreter.peek("b") should be (BigInt(-7))

    interpreter.step(1)
    interpreter.peek("c") should be (BigInt(-8))

    interpreter.poke("a", 255)
    interpreter.peek("a") should be (BigInt(-1))
    interpreter.poke("b", 249)
    interpreter.peek("b") should be (BigInt(-7))

    interpreter.step(1)
    interpreter.peek("c") should be (BigInt(-8))
    interpreter.report()

  }

  behavior of "Using VCD output as a golden model test of a circuit"

  it should "be able to create a VCD then replay the VCD testing inputs" in {
    val stream = getClass.getResourceAsStream("/VcdAdder.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")

    val manager = new InterpreterOptionsManager {
      interpreterOptions = interpreterOptions.copy(writeVCD = true)
      commonOptions = CommonOptions(targetDirName = "test_run_dir")
    }

    val interpreter = new InterpretiveTester(input, manager)
    interpreter.step()
    interpreter.poke("io_a", 3)
    interpreter.poke("io_b", 5)
    interpreter.peek("io_a") should be (BigInt(3))
    interpreter.peek("io_b") should be (BigInt(5))

    interpreter.step()
    interpreter.peek("io_c") should be (BigInt(8))

    //    interpreter.poke("io_a", -1)
//    interpreter.poke("io_b", -7)
//    interpreter.peek("io_a") should be (BigInt(-1))
//    interpreter.peek("io_b") should be (BigInt(-7))
//
//    interpreter.step()
//    interpreter.peek("io_c") should be (BigInt(-8))

    interpreter.report()
  }
}
