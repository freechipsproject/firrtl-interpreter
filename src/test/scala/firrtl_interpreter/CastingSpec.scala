// See LICENSE for license details.

package firrtl_interpreter

import firrtl.PrimOps.{AsUInt, AsSInt}
import firrtl.ir._

import org.scalatest.{Matchers, FlatSpec}

// scalastyle:off magic.number
class CastingSpec extends FlatSpec with Matchers {
  behavior of "requiredBitsForSInt"

  it should "return correct values as specified in list" in {
    val testData = Map(
      -9 -> 5, -8 -> 4, -7 -> 4, -6 -> 4, -5 -> 4, -4 -> 3, -3 -> 3, -2 -> 2, -1 -> 1,
      0 -> 1,
      1 -> 2, 2 -> 3, 3 -> 3, 4 -> 4, 5 -> 4, 6 -> 4, 7 -> 4, 8 -> 5, 9 -> 5
    )
    for((num, width) <- testData) {
      println(s"test $num expected $width expected ${requiredBitsForSInt(num)}")
      requiredBitsForSInt(num) should be (width)
    }
  }

  behavior of "requiredBitsForUInt"

  it should "return correct values as specified in list" in {
    val testData = Map(
      -9 -> 5, -8 -> 4, -7 -> 4, -6 -> 4, -5 -> 4, -4 -> 3, -3 -> 3, -2 -> 2, -1 -> 1,
      0 -> 1,
      1 -> 1, 2 -> 2, 3 -> 2, 4 -> 3, 5 -> 3, 6 -> 3, 7 -> 3, 8 -> 4, 9 -> 4
    )
    for((num, width) <- testData) {
      println(s"test $num expected $width expected ${requiredBitsForUInt(num)}")
      requiredBitsForUInt(num) should be (width)
    }
  }

  behavior of "casting ops"

  they should "be able to cast things to SInt and back again" in {
    val input =
      """circuit Test :
        |  module Test :
        |    input clk : Clock
        |    input a : UInt<1>
        |    output c : UInt<2>
        |    reg w : UInt<1>, clk
        |    w <= a
        |    c <= w
      """.stripMargin

    val interpreter = FirrtlTerp(input)
    val evaluator = new LoFirrtlExpressionEvaluator(interpreter.dependencyGraph, interpreter.circuitState)

    val u = ConcreteUInt(4, 3)
    val target = UIntLiteral(4, IntWidth(3))
    val s = evaluator.castingOp(AsSInt, Seq(target), SIntType(IntWidth(3)))
    println(s"Casted s $s")
    s.isInstanceOf[ConcreteSInt] should be (true)

    val ss = evaluator.castingOp(AsUInt, Seq(SIntLiteral(s.value, IntWidth(s.width))), UIntType(IntWidth(3)))
    ss.value should be (u.value)

    var testCount = 0
    for(i <- IntWidthTestValuesGenerator(1, InterpreterMaxSupportedWidth)) {
      var b1 = Big0
      val top = BigInt("1"*i, 2)
      for(b1 <- BigIntTestValuesGenerator(0, top)) {
        val uiv1 = UIntLiteral(b1, IntWidth(i))
        val scv1 = evaluator.castingOp(AsSInt, Seq(uiv1), SIntType(IntWidth(i)))
        val siv1 = SIntLiteral(scv1.value, IntWidth(scv1.width))
        val ucv2 = evaluator.castingOp(AsUInt, Seq(siv1), UIntType(IntWidth(i)))

        if(b1.testBit(i-1)) {
          scv1.value should be < Big0
          ((top + 1) + scv1.value) should be (b1)
        }
        else {
          scv1.value should be (b1)
        }
        ucv2.value should be (b1)

        testCount += 1
      }
    }
    println(s"Test count $testCount")
  }

}
