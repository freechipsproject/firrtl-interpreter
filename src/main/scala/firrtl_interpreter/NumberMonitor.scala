/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

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
class NumberMonitor(val name: String, val canBeNegative: Boolean, val bitSize: Int, numberOfBins: Int = 0) {
  var samples: Long = 0L
  var mu: Double = 0.0
  var sq: Double = 0.0

  val (minPossible, maxPossible) = if(canBeNegative) extremaOfSIntOfWidth(bitSize) else extremaOfUIntOfWidth(bitSize)
  val adjustedMaxPossible: BigInt = maxPossible - minPossible

  // These are the minimum and maximum values seen by update
  var minValue: BigInt    = BigInt(1) << (bitSize - 1)
  var maxValue: BigInt    = -(BigInt(1) << (bitSize - 1)) - BigInt(1)

  val bins: Array[Long] = Array.fill(numberOfBins)(0L)

  val divisor: BigInt = {
    if(numberOfBins > 0 && adjustedMaxPossible > numberOfBins ) {
      (adjustedMaxPossible + 1) / BigInt(numberOfBins)
    }
    else {
      BigInt(1)
    }
  }
  private val doBinning = if(numberOfBins > 2) updateBins _ else noBinning _

  def noBinning(value: BigInt): Unit = Unit
  def updateBins(value: BigInt): Unit = {
    val adjustedValue = value - minPossible
    val fixedValue = if(adjustedValue < 0) {
      println(s"instrumenting $name: got bad low value = $value adjusted $adjustedValue")
      Big0
    }
    else if(adjustedValue > adjustedMaxPossible) {
      println(s"instrumenting $name: got bad high value = $value adjusted $adjustedValue")
      adjustedMaxPossible
    }
    else {
      adjustedValue
    }

    val intIndex = (fixedValue / divisor).toInt
    bins(intIndex) += 1
  }

  def clear(): Unit = {
    samples = 0
    mu = 0.0
    sq = 0.0
    minValue = BigInt(1) << (bitSize - 1)
    maxValue = -(BigInt(1) << (bitSize - 1) - 1)
  }

  def update(value: BigInt): Unit = {
    val double = value.toDouble

    // Next four lines use Welford's algorithm to compute running mean and variance
    samples += 1L
    val newMu = mu + ((double - mu) / samples.toDouble)
    sq += ((double - mu) * (double - newMu))
    mu = newMu

    if(value > maxValue) maxValue = value
    if(value < minValue) minValue = value

    doBinning(value)
  }

  def mean: Double = mu
  def variance: Double = if(samples > 1) sq / samples else 0.0
  def stddev: Double   = if(samples > 1) math.sqrt(variance) / samples else 0.0
  def showBins: String = bins.map { x => f"$x%8d" }.mkString(" ")

  def render(prettyPrint: Boolean = true): String = {
    def showType: String = s"""${if(canBeNegative) "sint<" else "uint<"}$bitSize>"""

    if(prettyPrint) {
      f"$showType%10s,$samples%8d,$minValue%8d,$maxValue%16d,$mean%16.5f," +
        f"$stddev%16.5f,${bins.map(x => f"$x%8d").mkString(",")}"
    }
    else {
      s"$showType,$samples,$minValue,$maxValue,$mean," +
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
  if(numberOfBins > 0 && binLogSize != binLogSize.toInt) {
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
            monitorRecord(key) =
              new NumberMonitor(key, concrete.isInstanceOf[ConcreteSInt], concrete.width, numberOfBins)
            monitorRecord(key)
        }
      }
      monitor.update(concrete.value)
    }
  }

  def renderHeader: String = {
    if(prettyPrintReport) {
      f"${"type"}%10s,${"n"}%8s,${"min"}%8s,${"max"}%16s,${"mean"}%16s," +
        f"${"stddev"}%16s${(0 until numberOfBins).map(x => f"${"bin" + x}%8s").mkString(",", ",", "")}"
    }
    else {
      s"${"type"},${"n"},${"min"},${"max"},${"mean"}," +
        s"${"stddev"}${(0 until numberOfBins).map(x => s"${"bin" + x}").mkString(",", ",", "")}"
    }
  }

  def monitorReport: String = {
    val headerString = monitorRecord.values.headOption match {
      case Some(_) =>
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
