// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{FreeSpec, Matchers}

class NumberMonitorSpec extends FreeSpec with Matchers {
  val rand = scala.util.Random
  val sampleSize = 10000000

  "NumberMonitor class computes a running average, variance, min, max and histogram of series of inputs" - {
    "constant inputs should create mean equal to constant, variance 0" in {
      val monitor = new NumberMonitor(canBeNegative = true, 16, 8)

      for(_ <- 0 until sampleSize) {
        val value = BigInt(42)
        monitor.update(value)
      }

      monitor.mean should be (42.0)
      monitor.variance should be (0.0)
      monitor.minValue should be (42.0)
      monitor.maxValue should be (42.0)

      println(s"mean = ${monitor.mean}, σ = ${monitor.variance} min = ${monitor.minValue} max = ${monitor.maxValue}")
      println(s"bins = ${monitor.showBins}")
    }
  }
  "even distribution inputs should create mean close to zero, variance 0" in {
    val monitor = new NumberMonitor(canBeNegative = true, 16, 8)

    for(_ <- 0 until sampleSize) {
      val value = rand.nextInt(1 << 16) + monitor.minPossible
      monitor.update(value)
    }

    println(s"mean = ${monitor.mean}, σ = ${monitor.variance} min = ${monitor.minValue} max = ${monitor.maxValue}")
    println(s"bins = ${monitor.showBins}")

//    monitor.mean.abs should be (5.0)
//    monitor.variance should be (0.0)
//    monitor.minValue should be (42.0)
//    monitor.maxValue should be (42.0)

    println(s"mean = ${monitor.mean}, σ = ${monitor.variance} min = ${monitor.minValue} max = ${monitor.maxValue}")
    println(s"bins = ${monitor.showBins}")
  }

  "gaussian distribution inputs should create mean close to zero, variance 0" in {
    val monitor = new NumberMonitor(canBeNegative = true, 16, 8)

    for(_ <- 0 until sampleSize) {
      val randomNum = (rand.nextGaussian() / 6.0) * (1 << 14)
      val value = BigInt(randomNum.toLong)
      monitor.update(value)
    }

    println(s"mean = ${monitor.mean}, σ = ${monitor.variance} min = ${monitor.minValue} max = ${monitor.maxValue}")
    println(s"bins = ${monitor.showBins}")

    //    monitor.mean.abs should be (5.0)
    //    monitor.variance should be (0.0)
    //    monitor.minValue should be (42.0)
    //    monitor.maxValue should be (42.0)

    println(s"mean = ${monitor.mean}, σ = ${monitor.variance} min = ${monitor.minValue} max = ${monitor.maxValue}")
    println(s"bins = ${monitor.showBins}")
  }

}
