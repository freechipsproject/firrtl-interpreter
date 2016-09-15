// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{Matchers, FreeSpec}

//scalastyle:off magic.number
class PoisonSpec extends FreeSpec with Matchers {
  "poison should propagate through evaluation" in {
    val input =
      """
        |circuit Poison :
        |  module Poison :
        |    input u_in : UInt<4>
        |    input s_in : SInt<4>
        |    output s_out: UInt<4>
        |
        |    node T_1 = add(u_in, s_in)
        |    node T_2 = bits(T_1, 3, 0)
        |    s_out <= T_2
      """.stripMargin

    val tester = new InterpretiveTester(input)

    //TODO: need to fix Firrtl issues #308 before this test
    //does not randomly blow up.
//    tester.interpreter.setVerbose(true)
//
//    tester.poke("u_in", Concrete.poisonedUInt(10))
//    tester.poke("s_in", Concrete.poisonedSInt(10))
//
//    tester.peekConcrete("s_out").toString should include ("P")
//
//    tester.poke("u_in", Concrete.poisonedUInt(10))
//    tester.poke("s_in", 10)
//
//    tester.peekConcrete("s_out").toString should include ("P")
//
//    tester.poke("u_in", 77)
//    tester.poke("s_in", Concrete.poisonedSInt(10))
//
//    tester.peekConcrete("s_out").toString should include ("P")
//
//    tester.poke("u_in", 10)
//    tester.poke("s_in", 10)
//
//    tester.peekConcrete("s_out").toString should not include "P"

//    val (i, j) = (BigInt(1), BigInt(15))
//    tester.poke("u_in", ConcreteUInt(j, 10))
//    tester.poke("s_in", ConcreteSInt(i, 10))
//    println(s"$i.S + $j.U => ${tester.peek("s_out")}")

//    val (lo, hi) = TestUtils.extremaOfSIntOfWidth(10)
//    val uhi: BigInt = TestUtils.allOnes(10)
//    for(i <- lo to hi) {
//      for(j <- Big0 to uhi) {
//        tester.poke("u_in", ConcreteUInt(j, 10))
//        tester.poke("s_in", ConcreteSInt(i, 10))
//        println(s"$i.S + $j.U => ${tester.peek("s_out")}")
//      }
//    }
  }
}
