// See LICENSE for license details.

package firrtl_interpreter.executable

import org.scalatest.{FreeSpec, Matchers}


// scalastyle:off magic.number
class BigOpsSpec extends FreeSpec with Matchers {
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

  "BigOps should pass a basic test" - {
    "AsUIntBigs should work" in {
      AsUIntBigs(fMinus6, width = 4)() should be (10)
      AsUIntBigs(() => -22, width = 128)() should be (
        BigInt("340282366920938463463374607431768211434", 10))
      AsUIntBigs(fMinus4, width = 4)() should be (12)

      AsUIntBigs(f0, width = 1)() should be (BigInt(0))
      AsUIntBigs(fMinus1, width = 1)() should be (BigInt(1))
      AsUIntBigs(f0, width = 1)() should be (BigInt(0))
      AsUIntBigs(f1, width = 1)() should be (BigInt(1))

      AsUIntBigs(f3, width = 3)() should be (BigInt(3))
      AsUIntBigs(fMinus4, width = 3)() should be (BigInt(4))
    }

    "AsSIntBigs should work" in {
      AsSIntBigs(f0, width = 1)() should be (BigInt(0))
      AsSIntBigs(fMinus1, width = 1)() should be (BigInt(-1))
      AsSIntBigs(f0, width = 1)() should be (BigInt(0))
      AsSIntBigs(f1, width = 1)() should be (BigInt(-1))

      AsSIntBigs(f3, width = 2)() should be (BigInt(-1))
      AsSIntBigs(f3, width = 3)() should be (BigInt(3))
    }

    "cat ops should combiapne bits from two numbers" in {
      CatBigs(f1, f1IsSigned = false, f1Width = 1, f1, f2IsSigned = false, f2Width = 1)() should be (BigInt(3))
      CatBigs(fMinus1, f1IsSigned = true,  f1Width = 2, f1, f2IsSigned = false, f2Width = 1)() should be (BigInt(7))

      CatBigs(fMinus1, f1IsSigned = true,  f1Width = 2,
        fMinus1, f2IsSigned = true, f2Width = 2)() should be (BigInt(15))
      CatBigs(fMinus1, f1IsSigned = true,  f1Width = 2,
        fMinus2, f2IsSigned = true, f2Width = 2)() should be (BigInt(14))
      CatBigs(f1, f1IsSigned = false,  f1Width = 1, fMinus1, f2IsSigned = true, f2Width = 2)() should be (BigInt(7))
    }

    "bit ops should take arbitrary bits from a value" in {
      BitsBigs(val2, isSigned = false, high = 1, low = 0, originalWidth = 8)() should be (BigInt(2))
      BitsBigs(val2, isSigned = false, high = 2, low = 0, originalWidth = 8)() should be (BigInt(2))
      BitsBigs(val2, isSigned = false, high = 3, low = 0, originalWidth = 8)() should be (BigInt(10))
      BitsBigs(val2, isSigned = false, high = 3, low = 1, originalWidth = 8)() should be (BigInt(5))
      BitsBigs(val2, isSigned = false, high = 3, low = 2, originalWidth = 8)() should be (BigInt(2))
      BitsBigs(val2, isSigned = false, high = 3, low = 3, originalWidth = 8)() should be (BigInt(1))
    }

    "head ops should take bits from front of number" in {
      HeadBigs(f1, takeBits = 1, originalWidth = 1)() should be (BigInt(1))
      HeadBigs(f1, takeBits = 1, originalWidth = 2)() should be (BigInt(0))

      HeadBigs(fMinus1, takeBits = 1, originalWidth = 1)() should be (BigInt(1))
      HeadBigs(fMinus1, takeBits = 1, originalWidth = 2)() should be (BigInt(1))
      HeadBigs(f1, takeBits = 1, originalWidth = 2)() should be (BigInt(0))
      HeadBigs(f2, takeBits = 1, originalWidth = 3)() should be (BigInt(0))

      HeadBigs(val1, takeBits = 9, originalWidth = 17)() should be (BigInt(Integer.parseInt("ab", 16)))
      HeadBigs(val1, takeBits = 8, originalWidth = 16)() should be (BigInt(Integer.parseInt("ab", 16)))

    }

    "tail ops should drop leading bits from expression" in {
      TailBigs(f1, toDrop = 1, originalWidth = 2)() should be (BigInt(1))
      TailBigs(f2, toDrop = 1, originalWidth = 3)() should be (BigInt(2))
      TailBigs(f3, toDrop = 1, originalWidth = 3)() should be (BigInt(3))
      TailBigs(f3, toDrop = 1, originalWidth = 2)() should be (BigInt(1))
      TailBigs(fMinus3, toDrop = 1, originalWidth = 4)() should be (BigInt(5))
      TailBigs(fMinus4, toDrop = 1, originalWidth = 4)() should be (BigInt(4))

      val tailOps = TailBigs(val1, toDrop = 9, originalWidth = 17)
      println(f"TailInts(${val1()}%x, toDrop = 8) -> ${tailOps()}%x")
      tailOps() should be (BigInt(Integer.parseInt("cd", 16)))
    }

  }
}
