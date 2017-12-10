// See LICENSE for license details.

package firrtl_interpreter.primops

import firrtl_interpreter.executable._
import firrtl_interpreter.{BigIntTestValuesGenerator, BitTwiddlingUtils, extremaOfUIntOfWidth, extremaOfSIntOfWidth}
import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class BitReductions extends FreeSpec with Matchers {
  "BitReductions should pass a basic test" - {
    "And reduction (Andr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = false).toInt
        AndrInts(() => input, isSigned = false, bitWidth).apply() should be(expected)
      }
    }

    "And reduction (Andr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = true).toInt
        AndrInts(() => input, isSigned = true, bitWidth).apply() should be(expected)
      }
    }

    "Or reduction (Orr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = false).toInt
        // println(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        OrrInts(() => input, isSigned = false, bitWidth).apply() should be(expected)
      }
    }

    "Or reduction (Orr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = true).toInt
        // println(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        OrrInts(() => input, isSigned = true, bitWidth).apply() should be(expected)
      }
    }

    "Xor reduction (Xorr) should work for uints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = false).toInt
        // println(s"input $input ${(input + 32).toBinaryString.takeRight(4)} expected $expected")
        XorrInts(() => input, isSigned = false, bitWidth).apply() should be(expected)
      }
    }

    "Xor reduction (Xorr) should work for sints with known examples" in {
      val bitWidth = 4
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for (i <- lo to hi) {
        val input = i.toInt
        val expected = BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = true).toInt
        // println(s"input $input ${(input + 1024).toBinaryString.takeRight(4)} expected $expected")
        XorrInts(() => input, isSigned = true, bitWidth).apply() should be(expected)
      }
    }

    "Reductions should pass for different bit widths when using UInt" in {
      for (size <- BigIntTestValuesGenerator((1, DataSize.LongThreshold * Big(2)))) {
        val bitWidth = size.toInt

        for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
          val (andrResult, orrResult, xorrResult) = DataSize(bitWidth) match {
            case IntSize =>
              (
                Big(AndrInts(() => i.toInt, isSigned = false, bitWidth).apply()),
                Big(OrrInts(()  => i.toInt, isSigned = false, bitWidth).apply()),
                Big(XorrInts(() => i.toInt, isSigned = false, bitWidth).apply())
              )
            case LongSize =>
              (
                Big(AndrLongs(() => i.toLong, isSigned = false, bitWidth).apply()),
                Big(OrrLongs(()  => i.toLong, isSigned = false, bitWidth).apply()),
                Big(XorrLongs(() => i.toLong, isSigned = false, bitWidth).apply())
              )
            case BigSize =>
              (
                AndrBigs(() => i, isSigned = false, bitWidth).apply(),
                OrrBigs(() => i,  isSigned = false, bitWidth).apply(),
                XorrBigs(() => i, isSigned = false, bitWidth).apply()
              )
          }
          val (andrExpected, orrExpected, xorrExpected) = (
            BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = false),
            BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = false),
            BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = false)
          )

          // println(s"bitWidth $bitWidth i $i orrResult $orrResult expected $orrExpected")

          andrResult should be(andrExpected)
          orrResult  should be(orrExpected)
          xorrResult should be(xorrExpected)
        }
      }
    }

    "Reductions should pass for different bit widths when using SInt" in {
      for (size <- BigIntTestValuesGenerator((1, DataSize.LongThreshold * Big(2)))) {
        val bitWidth = size.toInt

        for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
          val (andrResult, orrResult, xorrResult) = DataSize(bitWidth) match {
            case IntSize =>
              (
                Big(AndrInts(() => i.toInt, isSigned = true, bitWidth).apply()),
                Big(OrrInts(()  => i.toInt, isSigned = true, bitWidth).apply()),
                Big(XorrInts(() => i.toInt, isSigned = true, bitWidth).apply())
              )
            case LongSize =>
              (
                Big(AndrLongs(() => i.toLong, isSigned = true, bitWidth).apply()),
                Big(OrrLongs(()  => i.toLong, isSigned = true, bitWidth).apply()),
                Big(XorrLongs(() => i.toLong, isSigned = true, bitWidth).apply())
              )
            case BigSize =>
              (
                AndrBigs(() => i, isSigned = true, bitWidth).apply(),
                OrrBigs(() => i,  isSigned = true, bitWidth).apply(),
                XorrBigs(() => i, isSigned = true, bitWidth).apply()
              )
          }
          val (andrExpected, orrExpected, xorrExpected) = (
            BitTwiddlingUtils.andr(i, bitWidth, aIsSInt = true),
            BitTwiddlingUtils.orr(i, bitWidth, aIsSInt = true),
            BitTwiddlingUtils.xorr(i, bitWidth, aIsSInt = true)
          )

//          println(s"bitWidth $bitWidth i $i orrResult $orrResult expected $orrExpected")

          andrResult should be(andrExpected)
          orrResult  should be(orrExpected)
          xorrResult should be(xorrExpected)
        }
      }
    }
  }
}
