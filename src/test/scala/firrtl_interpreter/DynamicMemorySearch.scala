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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Created by chick on 5/4/16.
  */
class DynamicMemorySearch extends AnyFlatSpec with Matchers {
  behavior of "dynamic memory search"

  it should "run with correct results" ignore {
    val input =
    """
      |circuit DynamicMemorySearch :
      |  module DynamicMemorySearch :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    output io : {flip isWr : UInt<1>, flip wrAddr : UInt<3>, flip data : UInt<6>, flip en : UInt<1>, target : UInt<3>, done : UInt<1>}
      |
      |    io is invalid
      |    reg index : UInt<3>, clk with : (reset => (reset, UInt<3>("h00")))
      |    cmem list : UInt<6>[8]
      |    infer mport memVal = list[index], clk
      |    node T_10 = eq(io.en, UInt<1>("h00"))
      |    node T_11 = eq(memVal, io.data)
      |    node T_13 = eq(index, UInt<3>("h07"))
      |    node T_14 = or(T_11, T_13)
      |    node over = and(T_10, T_14)
      |    when io.isWr :
      |      infer mport T_15 = list[io.wrAddr], clk
      |      T_15 <= io.data
      |      skip
      |    node T_17 = eq(io.isWr, UInt<1>("h00"))
      |    node T_18 = and(T_17, io.en)
      |    when T_18 :
      |      index <= UInt<1>("h00")
      |      skip
      |    node T_21 = eq(over, UInt<1>("h00"))
      |    node T_23 = eq(io.isWr, UInt<1>("h00"))
      |    node T_25 = eq(io.en, UInt<1>("h00"))
      |    node T_26 = and(T_23, T_25)
      |    node T_27 = and(T_26, T_21)
      |    when T_27 :
      |      node T_29 = add(index, UInt<1>("h01"))
      |      node T_30 = tail(T_29, 1)
      |      index <= T_30
      |      skip
      |    io.done <= over
      |    io.target <= index
    """.stripMargin

    val n = 8
    val w = 4

    new InterpretiveTester(input) {
//      interpreter.setVerbose(true)
//      interpreter.sourceState.memories("list").setVerbose()

      val list = Array.fill(n)(0)
      random.setSeed(0L)

      // initialize memory
      for(write_address <- 0 until n) {
        println(s"Initializing memory address $write_address")
        poke("io_en", 0)
        poke("io_isWr", 1)
        poke("io_wrAddr", write_address)
        poke("io_data",   write_address)
        list(write_address) = write_address
        step(1)
      }

      println(s"${interpreter.circuitState.memories("list").toString}")

      for (k <- 0 until 160) {
        println(s"memory test iteration $k") // ${"X"*80}")

        // Compute a random address and value
        val wrAddr = random.nextInt(n - 1)
        val data   = random.nextInt((1 << w) - 1)
        println(s"setting memory($wrAddr) = $data")

        // poke it intro memory
        poke("io_en", 0)
        poke("io_isWr", 1)
        poke("io_wrAddr", wrAddr)
        poke("io_data",   data)
        list(wrAddr) = data
        step(1)

        // SETUP SEARCH
        val target = if (k > 12) random.nextInt(1 << w) else data
        poke("io_isWr", 0)
        poke("io_data", target)
        poke("io_en", 1)
        step(1)
        poke("io_en", 0)
        val expectedIndex = if (list.contains(target)) {
          list.indexOf(target)
        } else {
          list.length - 1
        }
        // println(s"test pass $k ${this.interpreter.circuitState.prettyString()}")

        var waitCount = 0
        while(waitCount <= n && peek("io_done") == Big0) {
          // println(s"Waiting for done $waitCount")
          // println(this.interpreter.circuitState.prettyString())
          // println(s"external list ${list.mkString(",")}")
          step(1)
          waitCount += 1
        }

        // println(s"Done wait count is $waitCount done is ${peek("io_done")} " +
        //   s"got ${peek("io_target")} got $expectedIndex")
        expect("io_done", 1)
        expect("io_target", expectedIndex)
        step(1)

      }
      report()
    }
  }
}
