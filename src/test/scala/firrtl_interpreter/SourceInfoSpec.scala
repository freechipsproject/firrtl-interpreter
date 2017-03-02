// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ExecutionOptionsManager
import org.scalatest.{FlatSpec, Matchers}

class SourceInfoSpec extends FlatSpec with Matchers {
  behavior of "source information"

  it should "be visible when logging and errors occur" in {
    val stream = getClass.getResourceAsStream("/FullAdder.ir")
    val input = io.Source.fromInputStream(stream).mkString

    val f = FirrtlTerp(input)

    f.evaluator.setVerbose(true)
    f.cycle()
    f.dependencyGraph.sourceInfo("a_and_b") should fullyMatch regex ".*FullAdder.scala 19:22.*"
  }

  it should "THIS COMPILES AND RUNS be included in concrete value instantiation" in {

    val input =
      """
        |circuit BadCircuit :
        |  module BadCircuit :
        |    input in1 : SInt<4>  @[BadCircuit.scala 1:22]
        |    input in2 : SInt<4>  @[BadCircuit.scala 2:22]
        |    output out : SInt<4>   @[BadCircuit.scala 3:22]
        |
        |    out <= sub(in1, in2)  @[BadCircuit.scala 4:22]
        |
      """.stripMargin

    val options = new ExecutionOptionsManager("firrtl-interpreter") with HasInterpreterOptions {}
    val interpreter = new InterpretiveTester(input, options)

    println(s"low firrtl ${interpreter.interpreter.loweredAst.serialize}")

    interpreter.poke("in1", -8)
    interpreter.poke("in2", 2)
    interpreter.peek("out") should be (-10)
  }
  it should "HEY THIS DOESN'T WORK. be included in concrete value instantiation" in {
    val input =
      """
        |circuit BadCircuit :
        |  module BadCircuit :
        |    input in1 : SInt<4> @[BadCircuit.scala 1:22]
        |    input in2 : SInt<4> @[BadCircuit.scala 2:22]
        |    output out : SInt<4> @[BadCircuit.scala 3:22]
        |
        |    skip
        |    out <= asSInt(bits(sub(in1, in2), 4, 0))
      """.stripMargin

    val options = new ExecutionOptionsManager("firrtl-interpreter") with HasInterpreterOptions {
      interpreterOptions = interpreterOptions.copy(lowCompileAtLoad = false)
    }
    val interpreter = new InterpretiveTester(input, options)

    println(s"low firrtl ${interpreter.interpreter.loweredAst.serialize}")

    interpreter.poke("in1", -8)
    interpreter.poke("in2", 2)
    interpreter.peek("out") should be (-10)
  }

}
