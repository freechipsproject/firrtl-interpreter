// See LICENSE for license details.

package firrtl_interpreter.executable

import org.scalatest.{FreeSpec, Matchers}

class DataStoreSpec extends FreeSpec with Matchers {
  "DataStore holds all state information" - {
    "can store data and retrieve for ints with 1 buffer" in {
      val numInts = 10
      val ds = new DataStore(numberOfInts = 10, numberOfLongs = 0, numberOfBigs = 0, numberOfBuffers = 1)

      val assigns = Seq.tabulate(numInts){ n => ds.AssignInt(n, () => n) }
      assigns.foreach { assign => assign() }

      val gets  = Seq.tabulate(numInts){ n => ds.GetInt(n) }
      gets.zipWithIndex.foreach { case (get, index) =>
        get() should be (index)
      }
      for(index <- 0 until numInts) {
        println(f"$index%4d" + ds.getIntRow(index).map { n => f"$n%5d" }.mkString(","))
      }
    }

    "can store data and retrieve for ints with 4 buffer" in {
      val numInts = 10
      val buffers = 4
      val ds = new DataStore(numberOfInts = numInts, numberOfLongs = 0, numberOfBigs = 0, numberOfBuffers = buffers)

      for(pass <- 0 until 20) {
        val assigns = Seq.tabulate(numInts) { n => ds.AssignInt(n, () => n * 7 + pass) }
        assigns.foreach { assign => assign() }
        ds.advanceBuffers()
      }

      for(index <- 0 until numInts) {
        println(f"$index%4d" + ds.getIntRow(index).map { n => f"$n%5d" }.mkString(","))
      }
    }
  }
}
