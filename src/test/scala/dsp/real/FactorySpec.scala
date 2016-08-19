// See LICENSE for license details.

package dsp.real

import firrtl_interpreter.{BlackBoxFactory, InterpretiveTester}
import org.scalatest.{Matchers, FlatSpec}

import scala.collection.mutable.ArrayBuffer

class FactorySpec extends FlatSpec with Matchers {
  behavior of "using dsp factory"

  it should "expand instances as found" in {
    val input = io.Source.fromFile("src/test/resources/RealAdder.fir").mkString

    val tester = new InterpretiveTester(input, blackBoxFactories = Seq(new DspRealFactory))
    tester.interpreter.verbose = true
    tester.interpreter.setVerbose(true)

    tester.poke("io_a1_node", 100)
    tester.poke("io_a2_node", 200)
    tester.step()

    println(s"output is ${tester.peek("io_c_node")}")

    tester.interpreter.dependencyGraph.outputPorts.size should be > 0
  }
}