// See LICENSE for license details.
package firrtl_interpreter

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by chick on 4/29/16.
  */
class GCDTester extends FlatSpec with Matchers {
  behavior of "GCD"

  val gcdFirrtl =
    """
      |circuit GCD :
      |  module GCD :
      |    input clk : Clock
      |    input reset : UInt<1>
      |    input io_a : UInt<32>
      |    input io_b : UInt<32>
      |    input io_e : UInt<1>
      |    output io_z : UInt<32>
      |    output io_v : UInt<1>
      |    reg x : UInt<32>, clk with :
      |      reset => (UInt<1>("h0"), x)
      |    reg y : UInt<32>, clk with :
      |      reset => (UInt<1>("h0"), y)
      |    node T_13 = gt(x, y)
      |    node T_14 = sub(x, y)
      |    node T_15 = tail(T_14, 1)
      |    node T_17 = eq(T_13, UInt<1>("h0"))
      |    node T_18 = sub(y, x)
      |    node T_19 = tail(T_18, 1)
      |    node T_21 = eq(y, UInt<1>("h0"))
      |    node GEN_0 = mux(T_13, T_15, x)
      |    x <= mux(io_e, io_a, GEN_0)
      |    node GEN_1 = mux(T_17, T_19, y)
      |    y <= mux(io_e, io_b, GEN_1)
      |    io_z <= x
      |    io_v <= T_21
    """.stripMargin


  it should "run with InterpretedTester" in {
    new InterpretiveTester(gcdFirrtl, vcdOutputFileName = "gcd.vcd") {
      // interpreter.setVerbose()
      step(1)
      poke("io_a", 34)
      poke("io_b", 17)
      poke("io_e", 1)
      step(1)

      poke("io_e", 0)
      step(1)

      while(peek("io_v") != Big1) {
        step(1)
      }
      expect("io_z", 17)

      writeVCD()
    }
  }
}
