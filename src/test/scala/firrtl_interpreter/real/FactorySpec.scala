// See LICENSE for license details.

package firrtl_interpreter.real

import firrtl_interpreter._
import org.scalatest.{Matchers, FlatSpec}

import scala.collection.mutable.ArrayBuffer

class FactorySpec extends FlatSpec with Matchers {
  behavior of "black box real adder"

  it should "expand instances as found" in {
//    val input = io.Source.fromFile("src/test/resources/RealAdder.fir").mkString
    val input = io.Source.fromFile("src/test/resources/DspAdder.fir").mkString

    val tester = new InterpretiveTester(input, blackBoxFactories = Seq(new DspRealFactory))
    tester.interpreter.verbose = true
    tester.interpreter.setVerbose(true)


    tester.poke("io_a1_node", doubleToBigIntBits(1.5))
    tester.poke("io_a2_node", doubleToBigIntBits(3.25))
    tester.step()

    val result = tester.peek("io_c_node")
    val doubleResult = bigIntBitsToDouble(result)
    println(s"output is ${doubleResult}")

    tester.interpreter.dependencyGraph.outputPorts.size should be > 0
  }
}