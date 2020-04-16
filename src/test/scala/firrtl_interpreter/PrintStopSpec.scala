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

class PrintStopSpec extends AnyFlatSpec with Matchers {
  behavior of "stop"

  it should "return not stop if condition is not met" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    stop(clk, UInt(0), 2) ; Can't happen!
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    for (_ <- 0 to 10) {
      interpreter.doCycles(2)
      interpreter.stopped should be (false)
    }
  }

  it should "return failure if a stop with non-zero result" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    stop(clk, UInt(1), 2) ; Failure!
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    intercept[StopException] {
      interpreter.doCycles(2)
    }
    interpreter.stopped should be (true)
    interpreter.stopResult should be (2)
  }

  it should "return success if a stop with zero result" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |    stop(clk, UInt(1), 0) ; Success!
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    intercept[StopException] {
      interpreter.doCycles(2)
    }
    interpreter.stopped should be (true)
    interpreter.stopResult should be (0)
  }

  behavior of "Print statement"

  it should "be visible" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |
        |    printf(clk, UInt(1), "HELLO WORLD\n")
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    interpreter.doCycles(2)

  }
  it should "support printf formatting" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |
        |    printf(clk, UInt(1), "HELLO WORLD int %d hex %x SIint %d\n", UInt(7), UInt(31), SInt(2) )
        |    printf(clk, UInt(1), "HELLO WORLD int %d hex %x SIint %d\n", UInt(7), UInt(31), SInt(-2) )
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    interpreter.doCycles(2)

    printf("Hello dog")

  }

  it should "support printf formatting with binary" in {
    val input =
      """
        |circuit Stop0 :
        |  module Stop0 :
        |    input clk : Clock
        |
        |    printf(clk, UInt(1), "char %c int %d hex %x SIint %d %b\n", UInt(77), UInt(7), UInt(255), SInt(-2), SInt(7) )
        |    printf(clk, UInt(1), "char %c int %d hex %x SIint %d %b\n", UInt(48), UInt(7), UInt(255), SInt(-2), SInt(-7) )
        |
      """.stripMargin

    val interpreter = FirrtlTerp(input)

    interpreter.doCycles(2)

  }
}
