// See LICENSE for license details.

package firrtl_interpreter.primops

import firrtl_interpreter.executable._
import firrtl_interpreter.{BigIntTestValuesGenerator, BitTwiddlingUtils, extremaOfSIntOfWidth, extremaOfUIntOfWidth}
import org.scalatest.{FreeSpec, Matchers}


//noinspection RedundantDefaultArgument
// scalastyle:off magic.number
class AsSIntAsUInt extends FreeSpec with Matchers {
  "AsSInt should pass some basic tests" - {
    "Should work for integer sizes" - {
      val bitWidth = 4
      "AsSInt should work right for UInts for known integer range" in {
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false).toInt
          AsSIntInts(() => input, isSigned = false, width = bitWidth).apply() should be(expected)
        }
      }
      "AsSInt should work right for SInts for known integer range" in {
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true).toInt
          AsSIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
        }
      }

      "AsSInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false).toInt
            AsSIntInts(() => input, isSigned = false, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsSInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true).toInt
            AsSIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
    "Should work for Long sizes" - {
      "AsSInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false).toLong
            AsSIntLongs(() => input, isSigned = false, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsSInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true).toLong
            AsSIntLongs(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }

    "Should work for Big sizes" - {
      "AsSInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = false)
            AsSIntBigs(() => input, isSigned = false, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsSInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asSInt(i, bitWidth, inputIsSInt = true)
            AsSIntBigs(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
  }

  "AsUInt should pass some basic tests" - {
    "Should work for integer sizes" - {
      val bitWidth = 4
      "AsUInt should work right for UInts for known integer range" in {
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          AsUIntInts(() => input, isSigned = false, width = bitWidth).apply() should be(i)
        }
      }
      "AsUInt should work right for SInts for known integer range" in {
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          val input = i.toInt
          val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = true).toInt
          AsUIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
        }
      }

      "AsUInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = false).toInt
            AsUIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsUInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toInt
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = true).toInt
            AsUIntInts(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
    "Should work for Long sizes" - {
      "AsUInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = false).toLong
            AsUIntLongs(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsUInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i.toLong
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = true).toLong
            AsUIntLongs(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }

    "Should work for Big sizes" - {
      "AsUInt should work right for UInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((DataSize.IntThreshold + 1, DataSize.LongThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfUIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = false)
            AsUIntBigs(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
      "AsUInt should work right for SInts for larger bit widths" in {
        for (size <- BigIntTestValuesGenerator((1, DataSize.IntThreshold))) {
          val bitWidth = size.toInt
          for (i <- BigIntTestValuesGenerator(extremaOfSIntOfWidth(bitWidth))) {
            val input = i
            val expected = BitTwiddlingUtils.asUInt(i, bitWidth, inputIsSInt = true)
            AsUIntBigs(() => input, isSigned = true, width = bitWidth).apply() should be(expected)
          }
        }
      }
    }
  }
}
