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

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

//scalastyle:off magic.number
class PoisonSpec extends AnyFreeSpec with Matchers {
  val TestWidth = 6

  "poison should propagate through evaluation" in {
    val input =
      s"""
        |circuit Poison :
        |  module Poison :
        |    input s_in1 : SInt<$TestWidth>
        |    input s_in2 : SInt<$TestWidth>
        |    output s_out: SInt<${TestWidth + 2}>
        |
        |    node T_1 = add(s_in1, s_in2)
        |    s_out <= T_1
      """.stripMargin

    val tester = new InterpretiveTester(input)

    //TODO: need to fix Firrtl issues #308 before this test
    //does not randomly blow up.
    // tester.interpreter.setVerbose(true)

    for(_ <- 0 to 100) {
      tester.poke("s_in1", Concrete.poisonedSInt(TestWidth))
      tester.poke("s_in2", Concrete.poisonedSInt(TestWidth))

      tester.peekConcrete("s_out").toString should include("P")

      tester.poke("s_in1", Concrete.poisonedSInt(TestWidth))
      tester.poke("s_in2", TestWidth)

      tester.peekConcrete("s_out").toString should include("P")

      tester.poke("s_in1", 17)
      tester.poke("s_in2", Concrete.poisonedSInt(TestWidth))

      tester.peekConcrete("s_out").showValue.contains("☠") should be (true)

      tester.peekConcrete("s_out").toString should include("P")

      tester.poke("s_in1", TestWidth)
      tester.poke("s_in2", TestWidth)

      tester.peekConcrete("s_out").toString should not include "P"
    }
  }
}
