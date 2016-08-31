// See LICENSE for license details.

package firrtl_interpreter.fixedpoint

import firrtl_interpreter._
import org.scalatest.{FlatSpec, Matchers}

class FactorySpec extends FlatSpec with Matchers {
  behavior of "dumb fixed point multiply test"

  it should "expand instances as found" in {
    val input =
      """circuit Unit :
        |  module Unit :
        |    input  a : Fixed<4><<1>>
        |    input  b : Fixed<6><<2>>
        |    output c : Fixed
        |    c <= mul(a, b)""".stripMargin

    val tester = new InterpretiveTester(input)
    tester.interpreter.verbose = true
    tester.interpreter.setVerbose(true)


    tester.poke("a", BigInt("10", 2))
    tester.poke("b", BigInt("100", 2))
    tester.step()

    tester.expect("c", BigInt("1000", 2))
  }
}