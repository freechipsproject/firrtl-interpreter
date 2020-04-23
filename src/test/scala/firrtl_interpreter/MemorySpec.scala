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

import firrtl.ir.{DefMemory, IntWidth, NoInfo, UIntType}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class MemorySpec extends AnyFlatSpec with Matchers {

  behavior of "Memory instances"

  it should "be creatable" in {
    val dataWidth = 42
    val depth = 17
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), depth, 1, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    memory.depth should be(depth)
    memory.readers.length should be(2)
    memory.readers.contains("read1") should be(true)
    memory.readers.contains("read2") should be(true)

    memory.writers.length should be(1)
    memory.writers.contains("write1") should be(true)

    memory.dataStore.length should be(depth)
  }

  it should "fields of a read port can be written then read" in {
    val dataWidth = 42
    val depth = 17
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1"), Seq("write1"), Seq()
    ))

    var key = "memory1.read1.en"

    key = "memory1.read1.data"
    for (value <- IntWidthTestValuesGenerator(0, depth)) {
      memory.setValue(key, ConcreteUInt(value, memory.addressWidth))
      memory.getValue(key).value should be(value)
    }
  }

  it should "fields of a write port can be written then read" in {
    val dataWidth = 42
    val depth = 17
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1"), Seq("write1"), Seq()
    ))


    var key = "memory1.write1.en"
    memory.setValue(key, ConcreteUInt(Big1, dataWidth))
    memory.getValue(key).value should be(Big1)
    memory.setValue(key, ConcreteUInt(Big0, dataWidth))
    memory.getValue(key).value should be(Big0)

    key = "memory1.write1.addr"
    for(value <- IntWidthTestValuesGenerator(0, depth)) {
      memory.setValue(key, ConcreteUInt(value, memory.addressWidth))
      memory.getValue(key).value should be(value)
    }
    key = "memory1.write1.data"
    for(value <- IntWidthTestValuesGenerator(1, 100)) {
      memory.setValue(key, ConcreteUInt(value, dataWidth))
      memory.getValue(key).value should be(value)
    }
  }

  it should "assign to memory by setting en, data, and addr" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    var lastValue = Big0
    val key = "memory1.write1"
    for (i <- 0 until memory.depth) {
      // println(s"Write test slot $i" + ("="*80))
      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(i, memory.addressWidth))
      memory.setValue(key + ".mask", ConcreteUInt(1, dataWidth))
      memory.setValue(key + ".data", ConcreteUInt(i * 2, dataWidth))
      memory.cycle()

      if(i > 0) memory.dataStore(i-1 % memory.depth).value should be(lastValue)
      lastValue = i * 2
    }
  }

  it should "read from memory by setting en, addr using read latency 0" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 0, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    val key = "memory1.read1"
    var lastValue = Big0

    for (i <- 0 until memory.depth) {
      memory.dataStore(i) = ConcreteUInt(i * 4, dataWidth)
    }

    // println(s"memory is ${memory.dataStore.map(_.value).mkString(",")}")

    lastValue = 999
    val staleValue = ConcreteUInt(lastValue, dataWidth)
    memory.setValue(key + ".data", staleValue)

    for (i <- 0 until memory.depth) {
      // println(s"Checking memory slot $i" + ("=" * 80))
      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(i, memory.addressWidth))
      // println("enable and address set")

      memory.cycle()
      memory.getValue(key + ".data").value should be (i * 4)

      lastValue = i * 4

      // println(s"got value $i ${memory.getValue(key+".data").value}")
    }
  }

  it should "read from memory by setting en, addr using read latency 1" in {
    val dataWidth = 42
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), 17, 1, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    val key = "memory1.read1"
    var lastValue = Big0

    for (i <- 0 until memory.depth) {
      memory.dataStore(i) = ConcreteUInt(i * 3, dataWidth)
    }

    // println(s"memory is ${memory.dataStore.map(_.value).mkString(",")}")

    lastValue = 999
    val staleValue = ConcreteUInt(lastValue, dataWidth)
    memory.setValue(key + ".data", staleValue)
    memory.getValue(key + ".data").value should be (staleValue.value)

    for (i <- 0 until memory.depth) {
      println(s"Checking memory slot $i" + ("=" * 80))
      memory.setValue(key + ".en", ConcreteUInt(1, 1))
      memory.setValue(key + ".addr", ConcreteUInt(i, memory.addressWidth))
      println("enable and address set")

//      println(memory)
      memory.cycle()
//      println(memory)
      memory.getValue(key + ".data").value should be(i * 3)

      println(s"got value $i ${memory.getValue(key + ".data").value}")
    }
  }

  it should "work combinationally when read delay is 0" in {
    val dataWidth = 64
    val depth     = 23
    val readDelay =  0

    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), depth, 1, readDelay, Seq("read1"), Seq("write1"), Seq()
    ))
    //      memory.setVerbose()
    val key = "memory1.read1"

    val testValue1 = 77
    memory.dataStore(3) = ConcreteUInt(testValue1, dataWidth)
    memory.dataStore(3).value should be(testValue1)

    val testValue2 = 55
    memory.dataStore(4) = ConcreteUInt(testValue2, dataWidth)
    memory.dataStore(4).value should be(testValue2)

    memory.setValue(key + ".en", ConcreteUInt(1, 1))
    memory.setValue(key + ".addr", ConcreteUInt(3, memory.addressWidth))
    memory.cycle()
    memory.getValue(key + ".data").value should be(testValue1)


    memory.setValue(key + ".addr", ConcreteUInt(4, memory.addressWidth))
    memory.getValue(key + ".data").value should be(testValue2)

    memory.getValue(key + ".data").value should be(testValue2)
  }

  it should "work  when read delay is 1" in {
    val dataWidth = 22
    val depth     = 44
    val readDelay =  1

    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), depth, 1, readDelay, Seq("read1"), Seq("write1"), Seq()
    ))
    //      memory.setVerbose()

    val key = "memory1.read1"

    val testValue1 = 77
    memory.dataStore(3) = ConcreteUInt(testValue1, dataWidth)
    memory.dataStore(3).value should be(testValue1)

    val testValue2 = 55
    memory.dataStore(4) = ConcreteUInt(testValue2, dataWidth)
    memory.dataStore(4).value should be(testValue2)

    memory.setValue(key + ".en", ConcreteUInt(1, 1))
    memory.setValue(key + ".addr", ConcreteUInt(3, memory.addressWidth))
    memory.getValue(key + ".data").value should not be testValue1
    memory.cycle()
    memory.getValue(key + ".data").value should be (testValue1)
    memory.setValue(key + ".addr", ConcreteUInt(4, memory.addressWidth))
    memory.cycle()
    memory.getValue(key + ".data").value should be(testValue2)

    memory.setValue(key + ".en", ConcreteUInt(0, 1))
    memory.getValue(key + ".data").value should be (testValue2)

    memory.cycle()
    memory.getValue(key + ".data").poisoned should be(true)

    memory.cycle()
    memory.getValue(key + ".data").poisoned should be(true)

    memory.setValue(key + ".en", ConcreteUInt(1, 1))
    memory.setValue(key + ".addr", ConcreteUInt(4, memory.addressWidth))
    memory.setValue(key + ".addr", ConcreteUInt(3, memory.addressWidth))
    memory.getValue(key + ".data").poisoned should be(true)

    memory.cycle()
    memory.getValue(key + ".data").value should be(testValue1)
  }

  it should "observe write delay 1" in {
    val dataWidth   = 64
    val depth       = 15
    val writeDelay  =  1
    val testValue1  = 53
    val testValue2  =  7
    val testAddress =  3

    println(s"testing write delay of $writeDelay ${"="*80}")
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)),
      depth, writeDelay, 1, Seq("read1", "read2"), Seq("write1"), Seq()
    ))

    val key = "memory1.write1"

    memory.dataStore(testAddress) = ConcreteUInt(testValue1, memory.dataWidth)
    memory.dataStore(testAddress).value should be (testValue1)

    memory.setValue(key + ".en", ConcreteUInt(1, 1))
    memory.setValue(key + ".mask", ConcreteUInt(1, 1))
    memory.setValue(key + ".addr", ConcreteUInt(testAddress, memory.addressWidth))
    memory.setValue(key + ".data", ConcreteUInt(testValue2, memory.dataWidth))

    memory.dataStore(testAddress).value should be (testValue1)

    note("we write to the write ports but nothing happens to the backing data store yet")
    memory.ports("write1").address should be (testAddress)
    memory.ports("write1").data.value should be (testValue2)
    memory.ports("write1").enable     should be (true)

    val pipeLine = memory.ports("write1").asInstanceOf[Memory#WritePort].pipeLine
    pipeLine(0).address should be (testAddress)
    pipeLine(0).data.value should be (testValue2)
    pipeLine(0).enable     should be (true)

    memory.cycle()
    memory.dataStore(testAddress).value should be (testValue2)

    memory.setValue(key + ".en",   ConcreteUInt(0, 1))
    memory.setValue(key + ".addr", ConcreteUInt(testAddress, memory.addressWidth))
    memory.setValue(key + ".data", ConcreteUInt(testValue1, memory.dataWidth))

    memory.ports("write1").address    should be (testAddress)
    memory.ports("write1").data.value should be (testValue1)
    memory.ports("write1").enable     should be (false)

    pipeLine(0).address should be (testAddress)
    pipeLine(0).data.value should be (testValue1)
    pipeLine(0).enable     should be (false)

    memory.dataStore(testAddress).value should be (testValue2)
    memory.cycle()
    memory.dataStore(testAddress).value should be (testValue2)
  }

  behavior of "read/write memory"

  it should "allow writing and reading to read/write port" in {
    val dataWidth = 64
    val depth = 15
    val writeDelay = 1
    val testValue1 = 53
    val testValue2 = 7
    val testAddress = 3

    val portKey     = "read_write"
    val key         = s"memory1.$portKey"

    println(s"testing write delay of $writeDelay ${"=" * 80}")
    val memory = Memory(DefMemory(
      NoInfo, "memory1", UIntType(IntWidth(dataWidth)), depth, writeDelay, 1, Seq(), Seq(), Seq("read_write")
    ))

    memory.dataStore(testAddress) = ConcreteUInt(testValue1, memory.dataWidth)
    memory.dataStore(testAddress).value should be(testValue1)

    memory.setValue(key + ".wmode", ConcreteUInt(1, 1))
    memory.setValue(key + ".en",    ConcreteUInt(1, 1))
    memory.setValue(key + ".mask",  ConcreteUInt(1, 1))
    memory.setValue(key + ".addr",  ConcreteUInt(testAddress, memory.addressWidth))
    memory.setValue(key + ".data",  ConcreteUInt(testValue2, memory.dataWidth))

    memory.dataStore(testAddress).value should be(testValue1)

    note("we write to the write ports but nothing happens to the backing data store yet")
    memory.ports(portKey).address should be(testAddress)
    memory.ports(portKey).data.value should be(testValue2)
    memory.ports(portKey).enable should be(true)

    val writePipeLine = memory.ports(portKey).asInstanceOf[Memory#ReadWritePort].writePipeLine
    writePipeLine(0).address should be(testAddress)
    writePipeLine(0).data.value should be(testValue2)
    writePipeLine(0).enable should be(true)

    memory.cycle()
    memory.dataStore(testAddress).value should be(testValue2)

    memory.setValue(key + ".en", ConcreteUInt(0, 1))
    memory.setValue(key + ".addr", ConcreteUInt(testAddress, memory.addressWidth))
    memory.setValue(key + ".data", ConcreteUInt(testValue1, memory.dataWidth))

    memory.ports(portKey).address should be(testAddress)
    memory.ports(portKey).data.value should be(testValue1)
    memory.ports(portKey).enable should be(false)

    writePipeLine(0).address should be(testAddress)
    writePipeLine(0).data.value should be(testValue1)
    writePipeLine(0).enable should be(false)

    memory.dataStore(testAddress).value should be(testValue2)
    memory.cycle()
    memory.dataStore(testAddress).value should be(testValue2)

    val testAddress2: BigInt = 5
    val readValue2: BigInt   = 11
    memory.dataStore(testAddress2.toInt) = ConcreteUInt(readValue2, dataWidth)


    memory.setValue(key + ".wmode", ConcreteUInt(0, 1))
    memory.setValue(key + ".en",    ConcreteUInt(1, 1))
    memory.setValue(key + ".addr",  ConcreteUInt(testAddress2, memory.addressWidth))
    memory.getValue(key + ".rdata").value should not be readValue2

    memory.cycle()

    memory.getValue(key + ".rdata").value should be (readValue2)
    memory.setValue(key + ".addr", ConcreteUInt(4, memory.addressWidth))
    memory.cycle()
    memory.getValue(key + ".rdata").value should be(Big0)

  }
}
