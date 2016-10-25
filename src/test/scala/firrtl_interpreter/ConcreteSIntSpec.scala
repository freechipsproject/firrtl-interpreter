// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number
class ConcreteSIntSpec extends FlatSpec with Matchers {
  behavior of "constructor"

  it should "throw exception if not enough bits to hold" in {
    intercept[InterpreterException] {
      ConcreteSInt(4, 2)
    }
  }

  it should "become a negative number if high bit of value is "
}
