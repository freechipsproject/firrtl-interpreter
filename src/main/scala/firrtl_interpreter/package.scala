// See LICENSE for license details.

//scalastyle:off package.object.name
package object firrtl_interpreter {

  import firrtl.ir._

  val InterpreterMaxSupportedWidth = 100

  val Big0 = BigInt(0)
  val Big1 = BigInt(1)
  val DangerShiftSize = 27
  val BitsRequiredOverflowSizeBigInt = Big1 << DangerShiftSize

  val random = new scala.util.Random
  random.setSeed(System.currentTimeMillis())

  def randomBigInt(width: Int): BigInt = {
    BigInt(width, random)
  }

  def boolToInt(condition: Boolean): Int = if(condition) 1 else 0
  def boolToBigInt(condition: Boolean): BigInt = if(condition) 1 else 0
  def widthToInt(width: Width): Int = width.asInstanceOf[IntWidth].width.toInt
  def typeToWidth(tpe: Type): Int = tpe match {
    case UIntType(w)  => widthToInt(w)
    case SIntType(w)  => widthToInt(w)
    case ClockType    => 1
    case other =>
      throw InterpreterException(s"Can't find width for unsupported type: $other")

  }
  def ceilingLog2(x: Int): Int = scala.math.ceil(scala.math.log(x) / scala.math.log(2)).toInt

//  /**
//    * give the minimum number required to hold @num adding one for sign as necessary
//    *
//    * @param num the number that must be contained
//    * @return
//    */
//  def requiredBits(num: BigInt): Int = {
//    // if(num > BitsRequiredOverflowSizeBigInt) {
//    //   throw new InterpreterException(s"Error:requiredBits num $num > $BitsRequiredOverflowSizeBigInt")
//    // }
//    if(num < 2) { 1 + (if(num < 0) 1 else 0) }
//    else if(num > BitsRequiredOverflowSizeBigInt) {
//      var width = DangerShiftSize
//      var comparison = Big1 << width
//      while(comparison <= num) {
//        width += 1
//        comparison <<= 1
//      }
//      width
//    }
//    else {
//      val a = num.abs.toDouble + 1.0
//      (scala.math.ceil(scala.math.log(a) / scala.math.log(2)) + (if(num < 0) 1.0 else 0.0)).toInt
//    }
//  }

  /**
    * Utility function that computes bits required for a number
    *
    * @param n number of interest
    * @return
    */
  def computeBits(n: BigInt): Int = {
    n.bitLength + (if(n < 0) 1 else 0)
  }
//  OLD VERSION IN CASE THE NEED CROPS UP AGAIN
//  def computeBits(n: BigInt): Int = {
//    if(n.abs > BitsRequiredOverflowSizeBigInt) {
//      val num = n.abs
//      var width = DangerShiftSize
//      var comparison = Big1 << width
//      while(comparison <= num) {
//        width += 1
//        comparison <<= 1
//      }
//      if(n > 0 ) {
//        width += 1
//      }
//      width
//    }
//    else {
//      n.bitLength + (if(n < 0) 1 else 0)
//    }
//  }

  /**
    * computes the smallest and largest values that will fit in an SInt
    * @param width width of SInt
    * @return tuple(minVale, maxValue)
    */
  def extremaOfSIntOfWidth(width: Int): (BigInt, BigInt) = {
    val nearestPowerOf2 = BigInt("1" + ("0" * (width - 1)), 2)
    (-nearestPowerOf2, nearestPowerOf2 - 1)
  }
  /**
    * computes the smallest and largest values that will fit in a UInt
    * @param width width of SInt
    * @return tuple(minVale, maxValue)
    */
  def extremaOfUIntOfWidth(width: Int): (BigInt, BigInt) = {
    if(width == 1) {
      (0, 1)
    }
    else {
      val nearestPowerOf2 = BigInt("1" + ("0" * (width - 1)), 2)
      (0, (nearestPowerOf2 * 2) - 1)
    }
  }

  /**
    * return the smallest number of bits required to hold the given number in
    * an SInt
    * Note: positive numbers will get one minimum width one higher than would be
    * required for a UInt
    *
    * @param num number to find width for
    * @return minimum required bits for an SInt
    */
  def requiredBitsForSInt(num: BigInt): Int = {
    if(num == Big0 || num == -Big1) {
      1
    }
    else {
      if (num < 0) {
        computeBits(num)
      }
      else {
        computeBits(num) + 1
      }
    }
  }

  /**
    * return the smallest number of bits required to hold the given number in
    * an UInt
    * Note: positive numbers will get one minimum width one higher than would be
    * required for a UInt
    *
    * @param num number to find width for
    * @return minimum required bits for an SInt
    */
  def requiredBitsForUInt(num: BigInt): Int = {
    if(num == Big0) {
      1
    }
    else {
      computeBits(num)
    }
  }

  def doubleToBigIntBits(double: Double): BigInt = {
    BigInt(java.lang.Double.doubleToLongBits(double))
  }

  def bigIntBitsToDouble(bigInt: BigInt): Double = {
    java.lang.Double.longBitsToDouble(bigInt.toLong)
  }


  trait SimpleLogger {
    var verbose = false
    def setVerbose(value: Boolean = true): Unit = {
      verbose = value
    }

    def log(msg: => String): Unit = {
      if(verbose) println(msg)
    }
  }
}
