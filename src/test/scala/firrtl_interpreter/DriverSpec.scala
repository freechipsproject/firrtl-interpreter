// See LICENSE for license details.

package firrtl_interpreter

import firrtl.FirrtlSourceAnnotation
import org.scalatest.{FreeSpec, Matchers}

class DriverSpec extends FreeSpec with Matchers {
  "The Driver class provides a simple caller with run-time parameters" - {
    "topName must be set" in {
      val input =
        """
          |circuit Dummy :
          |  module Dummy :
          |    input x : UInt<1>
          |    output y : UInt<1>
          |    y <= x
        """.stripMargin

      val source = FirrtlSourceAnnotation(input)
      val result = Driver.execute(Array("--fint-verbose"), Seq(source))
      result.isInstanceOf[InterpreterTesterCreated] should be (true)

      val tester = result.asInstanceOf[InterpreterTesterCreated].tester

      tester.poke("x", 1)
      tester.expect("y", 1)
    }
  }
}
