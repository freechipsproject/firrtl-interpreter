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
  def val2(): Int = Integer.parseInt("1" * 9, 2)
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

    "tail ops should drop leading bits from expression" in {
      TailInts(f1, isSigned = true, 1, 2)() should be (1)
      TailInts(f2, isSigned = false, 1, 3)() should be (2)
      TailInts(f3, isSigned = true, 1, 3)() should be (3)
      TailInts(f3, isSigned = true, 1, 2)() should be (1)
      TailInts(fMinus3, isSigned = true, 1, 3)() should be (3)
      TailInts(fMinus4, isSigned = true, 1, 3)() should be (0)

      val tailOps = TailInts(val1, isSigned = false, toDrop = 8, width = 16)
      println(f"TailInts(${val1()}%x, toDrop = 8) -> ${tailOps()}%x")
      tailOps() should be (Integer.parseInt("cd", 16))
    }

  }
}
