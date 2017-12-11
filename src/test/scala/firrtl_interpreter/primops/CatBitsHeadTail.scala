// See LICENSE for license details.

package firrtl_interpreter.primops

import firrtl_interpreter._
import firrtl_interpreter.BitTwiddlingUtils
import firrtl_interpreter.executable._
import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class CatBitsHeadTail extends FreeSpec with Matchers {
  def f0(): Int = 0
  def f1(): Int = 1
  def f2(): Int = 2
  def f3(): Int = 3
  def fMinus1(): Int = -1
  def fMinus2(): Int = -2
  def fMinus3(): Int = -3
  def fMinus4(): Int = -4
  def fMinus6(): Int = -6

  def val1(): Int = Integer.parseInt("abcd", 16)
  def val2(): Int = Integer.parseInt("10" * 4, 2)
  def val3(): Int = Integer.parseInt("0", 2)

  "Cat Bits Head and Tail should pass basic tests" - {
    "Head should bass the following tests" - {
      def doHeadCheck(i: Big, takeBits: Int, bitWidth: Int): Unit = {
        val got = (
          HeadInts(() => i.toInt,   takeBits, originalWidth = bitWidth).apply(),
          HeadLongs(() => i.toLong, takeBits, originalWidth = bitWidth).apply(),
          HeadBigs(() => i,         takeBits, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.head(i, takeBits, bitWidth),
          BitTwiddlingUtils.head(i, takeBits, bitWidth),
          BitTwiddlingUtils.head(i, takeBits, bitWidth)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "head should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for {
          takeBits <- 1 to bitWidth
          i <- lo to hi
        } {
          doHeadCheck(i, takeBits, bitWidth)
        }
      }
      "head should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for {
          takeBits <- 1 to bitWidth
          i <- lo to hi
        } {
          doHeadCheck(i, takeBits, bitWidth)
        }
      }
    }

    "Tail should pass following tests" - {
      def doTailCheck(i: Big, bitWidth: Int): Unit = {
        val got = (
          TailInts(() => i.toInt, toDrop = 1, originalWidth = bitWidth).apply(),
          TailLongs(() => i.toLong, toDrop = 1, originalWidth = bitWidth).apply(),
          TailBigs(() => i, toDrop = 1, originalWidth = bitWidth).apply()
        )
        val expected = (
          BitTwiddlingUtils.tail(i, 1, bitWidth),
          BitTwiddlingUtils.tail(i, 1, bitWidth),
          BitTwiddlingUtils.tail(i, 1, bitWidth)
        )

        // println(s"i $i got $got expected $expected")
        got should be(expected)
      }

      "tail should work on known range of sints" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfSIntOfWidth(bitWidth)

        for (i <- lo to hi) {
          doTailCheck(i, bitWidth)
        }
      }
      "tail should work on known range of uint" in {
        val bitWidth = 4
        val (lo, hi) = extremaOfUIntOfWidth(bitWidth)
        for (i <- lo to hi) {
          doTailCheck(i, bitWidth)
        }
      }
      "tail ops should drop leading bits from expression" in {
        TailInts(() => -22, toDrop = 1, originalWidth = 16)() should be(32746)

        TailInts(f1, toDrop = 1, originalWidth = 2)() should be(1)
        TailInts(f2, toDrop = 1, originalWidth = 3)() should be(2)
        TailInts(f3, toDrop = 1, originalWidth = 3)() should be(3)
        TailInts(f3, toDrop = 1, originalWidth = 2)() should be(1)
        TailInts(fMinus3, toDrop = 1, originalWidth = 4)() should be(5)
        TailInts(fMinus4, toDrop = 1, originalWidth = 4)() should be(4)

        val tailOps = TailInts(val1, toDrop = 9, originalWidth = 17)
        // println(f"TailInts(${val1()}%x, toDrop = 8) -> ${tailOps()}%x")
        tailOps() should be(Integer.parseInt("cd", 16))
      }
    }
  }
}
