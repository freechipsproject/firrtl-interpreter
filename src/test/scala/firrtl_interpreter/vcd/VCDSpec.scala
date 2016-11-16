// See LICENSE for license details.

package firrtl_interpreter.vcd

import org.scalatest.{Matchers, FlatSpec}

// scalastyle:off magic.number
class VCDSpec extends FlatSpec with Matchers {
  private def getVcd = {
    VCD("test_circuit")
  }

  behavior of "vcd"

  it should "be able to generate unique ids " in {
    val vcd = getVcd

    val ids = new collection.mutable.HashSet[String]
    for (i <- 0 to 1000) {
      val id = vcd.getIdString(i)

      ids.contains(id) should be(false)
      ids += id

      id.forall { c => c.toInt >= 33 && c.toInt <= 126 } should be(true)
    }
  }

  it should "allow add wires" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    vcd.wires.contains("bob") should be(true)
    vcd.wires.contains("carol") should be(true)
    vcd.wires.contains("ted") should be(true)

    vcd.wires.contains("alice") should be(false)
  }

  it should "ignore calls to wire changed when value has not changed" in {
    val vcd = getVcd

    vcd.addWire("bob", 4)
    vcd.addWire("carol", 16)
    vcd.addWire("ted", 3)

    // time starts at -1 to support initialized values
    vcd.incrementTime()
    for(i <- 0 to 10) {
      vcd.wireChanged("bob", i)
      vcd.wireChanged("carol", i / 2)
      vcd.wireChanged("ted", i / 4)
      vcd.incrementTime()
    }

    vcd.valuesAtTime(0).size should be (3)
    vcd.valuesAtTime(1).size should be (1)
    vcd.valuesAtTime(2).size should be (2)
    vcd.valuesAtTime(3).size should be (1)
    vcd.valuesAtTime(4).size should be (3)
    vcd.valuesAtTime(5).size should be (1)

    println(vcd.serialize)
  }

  behavior of "VCD reader"

  it should "be able to read a file" in {
    val vcdFile = VCD.read("src/test/resources/GCD.vcd")

    vcdFile.date should be ("2016-10-13T16:31+0000")
  }
}
