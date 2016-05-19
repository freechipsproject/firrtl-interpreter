// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{Matchers, FlatSpec}

class ModuleInLineSpec extends FlatSpec with Matchers {
  behavior of "multiple modes"

  it should "expand instances as found" in {
//    val input = io.Source.fromFile("src/test/resources/rocket.fir").mkString
    val input = io.Source.fromFile("src/test/resources/three_deep.fir").mkString

    val tester = new InterpretiveTester(input)

    tester.interpreter.dependencyGraph.outputPorts.size should be > 0
  }
}
