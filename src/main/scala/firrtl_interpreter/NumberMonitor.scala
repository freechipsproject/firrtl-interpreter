// See LICENSE for license details.

package firrtl_interpreter

/**
  * This tracks the running average, variance min, max, and a histogram of series of values
  * @param canBeNegative Are we tracking SInt values or UInt values
  * @param bitSize       How many bits to track
  * @param numberOfBins  Number of bits in the saved histogram, less than one disables binning
  */
class NumberMonitor(val canBeNegative: Boolean, val bitSize: Int, numberOfBins: Int = 0) {
  var samples: Long = 0L
  var mu: Double = 0.0
  var sq: Double = 0.0

  val minPossible: BigInt = if(canBeNegative) - (1 << (bitSize - 1)) else 0
  val maxPossible: BigInt = if(canBeNegative) (1 << (bitSize- 1)) - 1 else 1 << (bitSize - 1)
  var maxValue: BigInt    = BigInt(Long.MinValue)
  var minValue: BigInt    = BigInt(Long.MaxValue)

  private val bins = Array.fill(numberOfBins)(0L)
  private val binShift = (bitSize - (math.log(numberOfBins) / math.log(2))).toInt
  private val doBinning = if(numberOfBins > 2) updateBins _ else noBinning _

  def noBinning(value: BigInt): Unit = Unit
  def updateBins(value: BigInt): Unit = {
    bins((value >> binShift).toInt) += 1
  }

  def update(value: BigInt): Unit = {
    val adjustedValue = value - minPossible
    val double = value.toDouble

    // Next four lines use Welford's algorithm to compute running mean and variance
    samples += 1L
    val newMu = mu + ((double - mu) / samples.toDouble)
    sq += ((double - mu) * (double - newMu))
    mu = newMu

    if(value > maxValue) maxValue = value
    if(value < minValue) minValue = value

    if(adjustedValue < 0 ) {
      println(s"got bad value = $value adjusted $adjustedValue")
    }
    doBinning(adjustedValue)
  }

  def mean: Double = mu
  def variance: Double = if(samples > 1) sq / samples else 0.0
  def showBins: String = bins.map { x => f"$x%8d" }.mkString(" ")
}

object NumberMonitor {

}
