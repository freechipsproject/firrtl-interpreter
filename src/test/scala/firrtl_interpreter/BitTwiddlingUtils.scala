// See LICENSE for license details.

package firrtl_interpreter

/**
  * This object has an alternate way of computing the various primitive operations.
  * This creates a double check that the primitive operations are correct.
  * These are based on the original interpreter and were overall designed to be correct
  * rather than fast.
  */
object BitTwiddlingUtils {
  def plus(a: BigInt, b: BigInt, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): BigInt = {
    a + b
  }
  def minus(a: BigInt, b: BigInt, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): BigInt = {
    a - b
  }
  def times(a: BigInt, b: BigInt, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): BigInt = {
    a * b
  }
  def divide(a: BigInt, b: BigInt, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): BigInt = {
    a / b
  }
  def mod(a: BigInt, b: BigInt, bitWidth: Int = -1, aIsSInt: Boolean = true, bIsSInt: Boolean = true): BigInt = {
    a % b
  }
  def tail(a: BigInt, dropBits: Int, originalBitWidth: Int): BigInt = {
    var x = Big0
    val bitsWanted = originalBitWidth - dropBits
    for(i <- 0 until bitsWanted) {
      if(a.testBit(i)) x = x.setBit(i)
    }
    x
  }

  def asUInt(a: BigInt, bitWidth: Int, inputIsSInt: Boolean = false): BigInt = {
    if(inputIsSInt) {
      tail(a, dropBits = 0, originalBitWidth = bitWidth)
    }
    else {
      a
    }
  }

  def asSInt(a: BigInt, bitWidth: Int, inputIsSInt: Boolean = false): BigInt = {
    if(inputIsSInt) {
      a
    }
    else {
      val newValue = {
        if(a == Big1 && bitWidth == 1) {
          BigInt(-1)
        }
        else {
          var signCrossover = BigInt(1) << (bitWidth - 1)
          if(a >= signCrossover) {
            signCrossover <<= 1
            a - signCrossover
          }
          else {
            a
          }
        }
      }
      newValue
    }
  }

}
