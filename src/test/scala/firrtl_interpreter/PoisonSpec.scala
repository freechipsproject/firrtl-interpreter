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
        |    input u_in : UInt<$TestWidth>
        |    input s_in : SInt<$TestWidth>
        |    output s_out: SInt<${TestWidth + 2}>
        |
        |    node T_1 = add(u_in, s_in)
        |    s_out <= T_1
      """.stripMargin

    val tester = new InterpretiveTester(input)

    //TODO: need to fix Firrtl issues #308 before this test
    //does not randomly blow up.
    // tester.interpreter.setVerbose(true)

    tester.poke("u_in", Concrete.poisonedUInt(TestWidth))
    tester.poke("s_in", Concrete.poisonedSInt(TestWidth))

    tester.peekConcrete("s_out").toString should include ("P")

    tester.poke("u_in", Concrete.poisonedUInt(TestWidth))
    tester.poke("s_in", TestWidth)

    tester.peekConcrete("s_out").toString should include ("P")

    tester.poke("u_in", 17)
    tester.poke("s_in", Concrete.poisonedSInt(TestWidth))

    println(s"peek should show little skulls for poison ${tester.peekConcrete("s_out").showValue}")

    tester.peekConcrete("s_out").toString should include ("P")

    tester.poke("u_in", TestWidth)
    tester.poke("s_in", TestWidth)

    tester.peekConcrete("s_out").toString should not include "P"
  }
}
