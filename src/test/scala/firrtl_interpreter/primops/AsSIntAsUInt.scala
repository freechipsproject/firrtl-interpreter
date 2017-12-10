// See LICENSE for license details.

package firrtl_interpreter.primops

import firrtl_interpreter.executable.{AsUIntInts, DataSize}
import firrtl_interpreter.{BigIntTestValuesGenerator, BitTwiddlingUtils, extremaOfSIntOfWidth, extremaOfUIntOfWidth}
import org.scalatest.{FreeSpec, Matchers}


//noinspection RedundantDefaultArgument
// scalastyle:off magic.number
class AsSIntAsUInt extends FreeSpec with Matchers {
  "AsUInt should pass a basic test" - {
    val bitWidth = 4
    "AsUInt should work right for UInts for known integer range" in {
      val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
      for(i <- lo to hi) {
        val input = i.toInt
        AsUIntInts(() => input, isSigned = false, width = bitWidth).apply() should be (i)
      }
    }
    "AsUInt should work right for SInts for known integer range" in {
      val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
      for(i <- lo to hi) {val input = i.toInt
        val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = true).toInt
        AsUIntInts(() => input, isSigned = true, width = bitWidth).apply() should be (expected)
      }
    }

    "AsUInt should work right for UInts for larger bit widths" in {
      for(size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
        val bitWidth = size.toInt
        for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = false).toInt
          AsUIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
        }
      }
    }
    "AsUInt should work right for SInts for larger bit widths" in {
      for(size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
        val bitWidth = size.toInt
        for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = true).toInt
          AsUIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
        }
      }
    }
  }
}
