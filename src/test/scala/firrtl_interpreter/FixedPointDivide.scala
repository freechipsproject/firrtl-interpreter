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
  "Check adding numbers on DataSize transition boundaries" - {
    for(bitWidth <- Seq(16, 31, 32, 33, 63, 64, 65)) {
//    for(bitWidth <- Seq(3)) {
//    for(bitWidth <- Seq(32)) {
      s"Testing with width $bitWidth" in {

        val input =s"""
          |circuit SignedAdder : @[:@2.0]
          |  module SignedAdder : @[:@3.2]
          |    input clock : Clock @[:@4.4]
          |    input reset : UInt<1> @[:@5.4]
          |    input io_in0 : SInt<$bitWidth>
          |    input io_in1 : SInt<$bitWidth>
          |    output io_out : SInt<$bitWidth>
          |
          |    node _T_5 = add(io_in0, io_in1)
          |    node _T_6 = tail(_T_5, 1)
          |    node _T_7 = asSInt(_T_6)
          |    io_out <= _T_7
        """.stripMargin

        val tester = new

        InterpretiveTester(input)

        val mask = BigInt("1" * bitWidth, 2)

        for {
          i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))
          j <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))
//          j <- -10 to 10 by 10
//          i <- Seq(BigInt(-8)) ++ BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))
//          j <- Seq(-6) ++ (-10 to 10 by 10)
//          i <- Seq(-4)
//          j <- Seq(0)
        } {
          tester.poke("io_in0", i)
          tester.poke("io_in1", j)
          tester.expect("io_out", (i + j) & mask)
        }
      }
    }
  }
}
