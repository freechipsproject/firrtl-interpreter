// See LICENSE for license details.

package firrtl_interpreter.executable

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class IntOpsSpec extends FreeSpec with Matchers {
  def f0(): Int = 0
  def f1(): Int = 1
  def f2(): Int = 2
  def f3(): Int = 3
  def fMinus1(): Int = -1
  def fMinus2(): Int = -2
  def fMinus3(): Int = -3
  def fMinus4(): Int = -4

  def val1(): Int = Integer.parseInt("abcd", 16)
  def val2(): Int = Integer.parseInt("10" * 4, 2)
  def val3(): Int = Integer.parseInt("0", 2)

  "IntOps should pass a basic test" - {
    "AsUIntInts should work" in {
      AsUIntInts(f0, isSigned = true, width = 1)() should be (0)
      AsUIntInts(fMinus1, isSigned = true, width = 1)() should be (1)
      AsUIntInts(f0, isSigned = false, width = 1)() should be (0)
      AsUIntInts(f1, isSigned = false, width = 1)() should be (1)

      AsUIntInts(f3, isSigned = true, width = 3)() should be (3)
      AsUIntInts(fMinus4, isSigned = true, width = 3)() should be (4)
    }

    "AsSIntInts should work" in {
      AsSIntInts(f0, isSigned = true, width = 1)() should be (0)
      AsSIntInts(fMinus1, isSigned = true, width = 1)() should be (-1)
      AsSIntInts(f0, isSigned = false, width = 1)() should be (0)
      AsSIntInts(f1, isSigned = false, width = 1)() should be (-1)

      AsSIntInts(f3, isSigned = false, width = 2)() should be (-1)
      AsSIntInts(f3, isSigned = false, width = 3)() should be (3)
    }

    "bit ops should take arbitrary bits from a value" in {
      BitsInts(val2, isSigned = false, high = 1, low = 0, originalWidth = 8)() should be (2)
      BitsInts(val2, isSigned = false, high = 2, low = 0, originalWidth = 8)() should be (2)
      BitsInts(val2, isSigned = false, high = 3, low = 0, originalWidth = 8)() should be (10)
      BitsInts(val2, isSigned = false, high = 3, low = 1, originalWidth = 8)() should be (5)
      BitsInts(val2, isSigned = false, high = 3, low = 2, originalWidth = 8)() should be (2)
      BitsInts(val2, isSigned = false, high = 3, low = 3, originalWidth = 8)() should be (1)
    }

    "head ops should take bits from front of number" in {
      HeadInts(f1, isSigned = false, takeBits = 1, originalWidth = 1)() should be (1)
      HeadInts(f1, isSigned = false, takeBits = 1, originalWidth = 2)() should be (0)

      HeadInts(fMinus1, isSigned = true, takeBits = 1, originalWidth = 1)() should be (1)
      HeadInts(fMinus1, isSigned = true, takeBits = 1, originalWidth = 2)() should be (1)
      HeadInts(f1, isSigned = true, takeBits = 1, originalWidth = 2)() should be (0)
      HeadInts(f2, isSigned = true, takeBits = 1, originalWidth = 3)() should be (0)

      HeadInts(val1, isSigned = true, takeBits = 9, originalWidth = 17)() should be (Integer.parseInt("ab", 16))
      HeadInts(val1, isSigned = false, takeBits = 8, originalWidth = 16)() should be (Integer.parseInt("ab", 16))

    }

    "tail ops should drop leading bits from expression" in {
      TailInts(f1, isSigned = true,  toDrop = 1, originalWidth = 2)() should be (1)
      TailInts(f2, isSigned = false, toDrop = 1, originalWidth = 3)() should be (2)
      TailInts(f3, isSigned = true,  toDrop = 1, originalWidth = 3)() should be (3)
      TailInts(f3, isSigned = true,  toDrop = 1, originalWidth = 2)() should be (1)
      TailInts(fMinus3, isSigned = true, toDrop = 1, originalWidth = 4)() should be (5)
      TailInts(fMinus4, isSigned = true, toDrop = 1, originalWidth = 4)() should be (4)

      val tailOps = TailInts(val1, isSigned = true, toDrop = 9, originalWidth = 17)
      println(f"TailInts(${val1()}%x, toDrop = 8) -> ${tailOps()}%x")
      tailOps() should be (Integer.parseInt("cd", 16))
    }

  }
}
