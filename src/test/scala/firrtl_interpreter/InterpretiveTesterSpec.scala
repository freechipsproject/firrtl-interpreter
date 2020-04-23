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

class InterpretiveTesterSpec extends AnyFlatSpec with Matchers {
  behavior of "cycle mechanism"

  it should "mark circuit as stale after poke" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    input a : UInt<16>
        |    input b : UInt<16>
        |    output c : UInt<16>
        |
        |    reg reg1 : UInt<16>, clk
        |
        |    reg1 <= add(a, b)
        |    c <= add(reg1, UInt(1))
        |
      """.stripMargin

    val tester = new InterpretiveTester(input)
    val interpreter = tester.interpreter

    interpreter.circuitState.isStale should be (false)

    tester.poke("a", 1)

    interpreter.circuitState.isStale should be (true)

    tester.peek("c")

    interpreter.circuitState.isStale should be (false)
  }
}
