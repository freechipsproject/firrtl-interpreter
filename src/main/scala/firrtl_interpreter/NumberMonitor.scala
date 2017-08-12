// See LICENSE for license details.

package firrtl_interpreter

import scala.collection.mutable
import scala.util.matching.Regex

/**
  * This tracks the running average, variance min, max, and a histogram of series of values
  *
  * @param canBeNegative Are we tracking SInt values or UInt values
  * @param bitSize       How many bits to track
  * @param numberOfBins  Number of bits in the saved histogram, less than one disables binning
  */
class NumberMonitor(val canBeNegative: Boolean, val bitSize: Int, numberOfBins: Int = 0) {
  var samples: Long = 0L
  var mu: Double = 0.0
  var sq: Double = 0.0

  val minPossible: BigInt = if(canBeNegative) - (1L << (bitSize - 1)) else 0
  val maxPossible: BigInt = if(canBeNegative) (1L << (bitSize- 1)) - 1 else 1 << (bitSize - 1)
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
  def stddev: Double   = math.sqrt(variance)
  def showBins: String = bins.map { x => f"$x%8d" }.mkString(" ")

  def render(prettyPrint: Boolean = true): String = {
    if(prettyPrint) {
      f"$bitSize%6d,$samples%8d,$minValue%8d,$maxValue%16d,$mean%10.5f," +
        f"$stddev%10.5f,${bins.map(x => f"$x%8d").mkString(",")}"
    }
    else {
      s"$bitSize,$samples,$minValue,$maxValue,$mean," +
        s"$stddev,${bins.map(x => f"$x").mkString(",")}"
    }
  }
}

class MonitorManager(options: InterpreterOptions) {
  val monitorRecord: mutable.HashMap[String, NumberMonitor] = new mutable.HashMap()
  val excludeTemps: Boolean = ! options.monitorTrackTempNodes
  val LimitRegex: Regex = """^_?(GEN_|T_).*""".r
  val numberOfBins: Int = options.monitorHistogramBins
  val prettyPrintReport: Boolean = options.prettyPrintReport

  private val binLogSize = math.log(numberOfBins) / math.log(2)
  if(binLogSize != binLogSize.toInt) {
    throw InterpreterException(
      s"monitorHistogramBins $numberOfBins must be a power of 2, but is not"
    )
  }

  def includeThis(key: String) : Boolean = {
    LimitRegex.findFirstIn(key).isEmpty || ! excludeTemps
  }

  def monitorSetValue(key: String, concrete: Concrete): Unit = {
    if(! concrete.poisoned && includeThis(key)) {
      val monitor = {
        monitorRecord.get(key) match {
          case Some(m) => m
          case _ =>
            monitorRecord(key) = new NumberMonitor(concrete.isInstanceOf[ConcreteSInt], concrete.width, numberOfBins)
            monitorRecord(key)
        }
      }
      monitor.update(concrete.value)
    }
  }

  def renderHeader: String = {
    if(prettyPrintReport) {
      f"${"bits"}%6s,${"n"}%8s,${"min"}%8s,${"max"}%16s,${"mean"}%10s," +
        f"${"stddev"}%10s${(0 until numberOfBins).map(x => f"${"bin" + x}%8s").mkString(",", ",", "")}"
    }
    else {
      s"${"bits"},${"n"},${"min"},${"max"},${"mean"}," +
        s"${"stddev"}${(0 until numberOfBins).map(x => s"${"bin" + x}").mkString(",", ",", "")}"
    }
  }

  def monitorReport: String = {
    val headerString = monitorRecord.values.headOption match {
      case Some(monitor) =>
        if(prettyPrintReport) {
          f"${"key,"}%-40.40s$renderHeader\n"
        }
        else {
          s"${"key"},$renderHeader\n"
        }
      case _ => ""
    }

    headerString +
      monitorRecord.keys.toArray.sorted.map { key =>
        if(prettyPrintReport) {
          f"$key%-40.40s ${monitorRecord(key).render(prettyPrintReport)}"
        }
        else {
          s"$key,${monitorRecord(key).render(prettyPrintReport)}"
        }
      }.mkString("\n")
  }

}
