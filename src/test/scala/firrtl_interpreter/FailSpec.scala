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

class FailSpec extends AnyFlatSpec with Matchers {
  behavior of "explict fail"

  it should "fail a test with an explicit failure code" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input  a : Fixed<4><<1>>
        |    input  b : Fixed<6><<2>>
        |    output c : Fixed
        |    c <= mul(a, b)""".stripMargin

    val tester = new InterpretiveTester(input)

    tester.fail(3)
    tester.report()
    tester.reportString should include ("Failed: Code 3")
  }

  behavior of "test exception"

  it should "throw an InterpreterException" in {
    val input =
      """
        |circuit LIE :
        |  module LIE :
        |    node L_2 = UInt<8>(42)
        """.stripMargin
    val tester = new InterpretiveTester(input)
    // This will fail because L_2 isn't a port and nameToConcreteValue will return None
    // We should really provoke an exception that reports a failure,
    //  but that appears to be impossible currently.
    val exception = intercept[InterpreterException] {
      tester.peek("L_2")
    }

    exception.getMessage should include("L_2")
  }
}
