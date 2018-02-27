// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{Matchers, FreeSpec}

//scalastyle:off magic.number
class PoisonSpec extends FreeSpec with Matchers {
  val TestWidth = 6

  "poison should propagate through evaluation" in {
    val input =
      s"""
        |circuit Poison :
        |  module Poison :
        |    input s_in1 : SInt<$TestWidth>
        |    input s_in2 : SInt<$TestWidth>
        |    output s_out: SInt<${TestWidth + 2}>
        |
        |    node T_1 = add(s_in1, s_in2)
        |    s_out <= T_1
      """.stripMargin

    val tester = new InterpretiveTester(input)

    //TODO: need to fix Firrtl issues #308 before this test
    //does not randomly blow up.
    // tester.interpreter.setVerbose(true)

    for(_ <- 0 to 100) {
      tester.poke("s_in1", Concrete.poisonedSInt(TestWidth))
      tester.poke("s_in2", Concrete.poisonedSInt(TestWidth))

      tester.peekConcrete("s_out").toString should include("P")

      tester.poke("s_in1", Concrete.poisonedSInt(TestWidth))
      tester.poke("s_in2", TestWidth)

      tester.peekConcrete("s_out").toString should include("P")

      tester.poke("s_in1", 17)
      tester.poke("s_in2", Concrete.poisonedSInt(TestWidth))

      tester.peekConcrete("s_out").showValue.contains("☠") should be (true)

      tester.peekConcrete("s_out").toString should include("P")

      tester.poke("s_in1", TestWidth)
      tester.poke("s_in2", TestWidth)

      tester.peekConcrete("s_out").toString should not include "P"
    }
  }
}
