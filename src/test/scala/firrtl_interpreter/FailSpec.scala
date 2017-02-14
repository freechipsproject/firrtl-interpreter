// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{FlatSpec, Matchers}

class FailSpec extends FlatSpec with Matchers {
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
