// See LICENSE for license details.

package firrtl_interpreter.primops

import firrtl_interpreter.{BitTwiddlingUtils, InterpretiveTester, extremaOfSIntOfWidth, extremaOfUIntOfWidth}
import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class AndOrXor extends FreeSpec with Matchers {
  "And should show correct values, internal values are different" in {
    val input =
      """
        |circuit Ander :
        |  module Ander :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input a : SInt<4>
        |    input b : SInt<4>
        |    output c : UInt<4>
        |    c <= and(a, b)
        |
      """.stripMargin
    val bitWidth = 4
    val tester = new InterpretiveTester(input)
    val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
//    val (lo, hi) = (-7, -7)
    for {
      a <- lo to hi
      b <- lo to hi
    } {
      tester.poke("a", a)
      tester.poke("b", b)
      val expected = BitTwiddlingUtils.and(a, b, bitWidth)
      println(s"and test $a & $b => ${tester.peek("c")}")
      tester.expect("c", (a & b) & 0xf)
    }
    tester.report()
  }

  "And should show correct values" in {
    val input =
      """
        |circuit Ander :
        |  module Ander :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input a : SInt<4>
        |    input b : SInt<4>
        |    output c : UInt<4>
        |    c <= and(a, b)
        |
      """.stripMargin
    val tester = new InterpretiveTester(input)
    val (lo, hi) = extremaOfSIntOfWidth(4)
    for {
      a <- lo to hi
      b <- lo to hi
    } {
      tester.poke("a", a)
      tester.poke("b", b)
      tester.expect("c", (a & b) & 0xf)
    }
    tester.report()
  }

//  "And should work for uints with known examples of UInts" in {
//    val bitWidth = 4
//    val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
//    for {
//      i <- lo to hi
//      j <- lo to hi
//    } {
//      val a = i.toInt
//      val b = j.toInt
//      val expected = BitTwiddlingUtils.and(a, b, bitWidth, aIsSInt = false).toInt
//      println(f"inputs $a%5d (${(a + 32).toBinaryString.takeRight(4)})" +
//        f" $b%5d (${(b + 32).toBinaryString.takeRight(4)})" +
//        f" $expected%5d (${(expected + 32).toBinaryString.takeRight(4)})")
//
//      AndInts(() => a, () => b).apply() should be(expected)
//    }
//  }
//  "And should work for uints with known examples of SInts" in {
//    val bitWidth = 4
//    val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
//    for {
//      i <- lo to hi
//      j <- lo to hi
//    } {
//      val a = i.toInt
//      val b = j.toInt
//      val primpOp = AndInts(() => a, () => b).apply _
//      val result = AsUIntInts(primpOp, isSigned = false, bitWidth)
//      val expected = BitTwiddlingUtils.and(a, b, bitWidth, aIsSInt = true).toInt
//      val naive = AsUIntInts(() => a & b, isSigned = true, bitWidth).apply()
//      println(f"inputs $a%5d (${(a + 32).toBinaryString.takeRight(4)})" +
//        f" $b%5d (${(b + 32).toBinaryString.takeRight(4)})" +
//        f" $naive%5d (${(naive + 32).toBinaryString.takeRight(4)})" +
//        f" $expected%5d (${(expected + 32).toBinaryString.takeRight(4)})")
//
//      naive should be (expected)
//
//      AndInts(() => a, () => b).apply() should be(expected)
//    }
//  }
}
