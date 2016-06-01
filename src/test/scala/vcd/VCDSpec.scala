// See LICENSE for license details.

package vcd

import org.scalatest.{Matchers, FlatSpec}

class VCDSpec extends FlatSpec with Matchers {
  behavior of "vcd"

  it should "be created by a factory" in {
    val vcd = (new VCDFactory)("test_circuit")

    println(s"${vcd.date}")

    println(s"${vcd.serialize}")

    println(VCD.idChars.mkString(""))

    vcd.getIdString(0) should be ("!")
    vcd.getIdString(526) should be ("&Y")

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    println(vcd.serialize)

    for(i <- 0 to 10) {
      vcd.wireChanged("bob", i)
      vcd.wireChanged("carol", i / 2)
      vcd.wireChanged("ted", i / 4)
      vcd.incrementTime()
    }

    println(vcd.serialize)
  }
}
