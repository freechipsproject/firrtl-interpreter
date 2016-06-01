// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{Matchers, FlatSpec}

class SourceInfoSpec extends FlatSpec with Matchers {
  val input = io.Source.fromFile("src/test/resources/FullAdder.ir").mkString

  val f = FirrtlTerp(input)

  f.dependencyGraph.validNames.contains("a_xor_b") should be (true)

}
