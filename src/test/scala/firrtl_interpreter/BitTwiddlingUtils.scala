// See LICENSE for license details.

package firrtl_interpreter

import firrtl_interpreter.executable.Big
import firrtl_interpreter.utils.BitMasks

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

  def and(a: BigInt, b: BigInt, outputBitWidth: Int = -1): BigInt = {
    val uIntA = asUInt(a, outputBitWidth)
    val uIntB = asUInt(b, outputBitWidth)
    uIntA & uIntB
  }

  def andr(a: BigInt, bitWidth: Int, aIsSInt: Boolean): BigInt = {
    val uInt = asUInt(a, bitWidth)
    boolToBigInt((0 until bitWidth).map(i => uInt.testBit(i)).reduce(_&&_))
  }

  def orr(a: BigInt, bitWidth: Int, aIsSInt: Boolean): BigInt = {
    if(aIsSInt) {
      if(a < 0) { Big1 }
      else if(a != 0) { Big1 }
      else { Big0 }
    }
    else {
      val bits = (0 until bitWidth).map(i => a.testBit(i))
      boolToBigInt(bits.reduce(_||_))
    }
  }

  def xorr(a: BigInt, bitWidth: Int, aIsSInt: Boolean): BigInt = {
    val uInt = asUInt(a, bitWidth)
    boolToBigInt((0 until bitWidth).map(i => a.testBit(i)).reduce(_^_))
  }

  def bits(a: BigInt, high: Int, low: Int, originalBitWidth: Int): BigInt = {
    var x = Big0
    for(i <- 0 until (high - low) + 1) {
      if(a.testBit(i + low)) x = x.setBit(i)
    }
    x
  }

  def head(a: BigInt, takeBits: Int, originalBitWidth: Int): BigInt = {
    var x = Big0
    val bitOffset = originalBitWidth - takeBits
    for(i <- 0 until takeBits) {
      if(a.testBit(i + bitOffset)) x = x.setBit(i)
    }
    x
  }

  def tail(a: BigInt, dropBits: Int, originalBitWidth: Int): BigInt = {
    var x = Big0
    val bitsWanted = originalBitWidth - dropBits
    for(i <- 0 until bitsWanted) {
      if(a.testBit(i)) x = x.setBit(i)
    }
    x
  }

  def asUInt(a: BigInt, bitWidth: Int): BigInt = {
    val bitMasks = BitMasks.getBitMasksBigs(bitWidth)

    a & bitMasks.allBitsMask
  }

  def makeUInt(a: BigInt, bitWidth: Int): BigInt = {
    val b = a & BitMasks.getBitMasksBigs(bitWidth).allBitsMask
    b
  }

  def makeSInt(a: BigInt, bitWidth: Int): BigInt = {
    val masks = BitMasks.getBitMasksBigs(bitWidth)
    val b = a & masks.allBitsMask
    if(masks.isMsbSet(b)) {
      b - masks.nextPowerOfTwo
    }
    else {
      b
    }
  }

  def asSInt(a: BigInt, bitWidth: Int, inputIsSInt: Boolean = false): BigInt = {

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
