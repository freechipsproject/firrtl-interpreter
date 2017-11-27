// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class FixedPointDivide extends FreeSpec with Matchers {
  "FixedPointDivide should pass a basic test" in {
    val input =
      """
        |circuit FixedPointDivide : @[:@2.0]
        |  module FixedPointDivide : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |    input io_in : SInt<64> @[:@6.4]
        |    output io_out : SInt<64> @[:@6.4]
        |
        |    node _T_2 = asUInt(io_in) @[FixedPointSpec.scala 39:20:@8.4]
        |    node _T_3 = shr(_T_2, 2) @[FixedPointSpec.scala 39:27:@9.4]
        |    node _T_4 = asSInt(_T_3) @[FixedPointSpec.scala 39:55:@10.4]
        |    io_out <= _T_4
        |
      """.stripMargin

    val tester = new InterpretiveTester(input)

    tester.poke("io_in", 256)
    tester.expect("io_out", 64)
    tester.report()
  }
}

// scalastyle:off magic.number
class SignedAdder extends FreeSpec with Matchers {
  "FixedPointDivide should pass a basic test" in {
    val input =
      """
        |circuit SignedAdder : @[:@2.0]
        |  module SignedAdder : @[:@3.2]
        |    input clock : Clock @[:@4.4]
        |    input reset : UInt<1> @[:@5.4]
        |    input io_in0 : SInt<16> @[:@6.4]
        |    input io_in1 : SInt<16> @[:@6.4]
        |    output io_out : SInt<16> @[:@6.4]
        |
        |    node _T_5 = add(io_in0, io_in1) @[Adder.scala 89:20:@8.4]
        |    node _T_6 = tail(_T_5, 1) @[Adder.scala 89:20:@9.4]
        |    node _T_7 = asSInt(_T_6) @[Adder.scala 89:20:@10.4]
        |    io_out <= _T_7
      """.stripMargin

    val tester = new InterpretiveTester(input)

    tester.poke("io_in0", -10)
    tester.poke("io_in1", -10)
    tester.expect("io_out", -20)
    tester.report()
  }
}
