// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{Matchers, FreeSpec}

//scalastyle:off magic.number
class PoisonSpec extends FreeSpec with Matchers {
  "poison should propagate through evaluation" in {
    val input =
      """
        |circuit Poison :
        |  module Poison :
        |    input u_in : UInt<10>
        |    input s_in : SInt<10>
        |    output s_out: UInt<7>
        |
        |    node T_1 = add(u_in, s_in)
        |    node T_2 = bits(T_1, 5, 0)
        |    s_out <= T_2
      """.stripMargin

    val tester = new InterpretiveTester(input)

    tester.poke("u_in", Concrete.poisonedUInt(10))
    tester.poke("s_in", Concrete.poisonedSInt(10))

    tester.peekConcrete("s_out").toString should include ("P")

    tester.poke("u_in", Concrete.poisonedUInt(10))
    tester.poke("s_in", 10)

    tester.peekConcrete("s_out").toString should include ("P")

    tester.poke("u_in", 77)
    tester.poke("s_in", Concrete.poisonedSInt(10))

    tester.peekConcrete("s_out").toString should include ("P")

    tester.poke("u_in", 10)
    tester.poke("s_in", 10)

    tester.peekConcrete("s_out").toString should not include "P"
  }
}
