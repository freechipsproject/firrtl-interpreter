// See LICENSE for license details.
package firrtl_interpreter

import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number

class GCDTester extends FlatSpec with Matchers {
  behavior of "GCD"

  val gcdFirrtl: String =
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
    val tester = new InterpretiveTester(gcdFirrtl)
    // interpreter.setVerbose()
    List((1, 1, 1), (34, 17, 17), (8, 12, 4)).foreach { case (x, y, z) =>
      tester.step()
      tester.poke("io_a", x)
      tester.poke("io_b", y)
      tester.poke("io_e", 1)
      tester.step()

      tester.poke("io_e", 0)
      tester.step()

      while (tester.peek("io_v") != Big1) {
        tester.step()
      }
      tester.expect("io_z", z)
    }
  }

  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while(y > 0 ) {
      if (x > y) {
        x -= y
      }
      else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }

  //scalastyle:off method.length
  def manyValuesTest(width: Int) {
    val gcdFirrtl: String =
      s"""
         |circuit GCD :
         |  module GCD :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    input io_a : UInt<$width>
         |    input io_b : UInt<$width>
         |    input io_e : UInt<1>
         |    output io_z : UInt<$width>
         |    output io_v : UInt<1>
         |    reg x : UInt<$width>, clock with :
         |      reset => (UInt<1>("h0"), x)
         |    reg y : UInt<$width>, clock with :
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
    """
              .stripMargin

    val manager = new InterpreterOptionsManager
    val values =
      for {x <- 10 to 100
           y <- 1 to 100
      } yield (x, y, computeGcd(x, y)._1)

    val tester = new InterpretiveTester(gcdFirrtl, manager)

    val startTime = System.nanoTime()
    tester.poke("clock", 1)

    for((x, y, z) <- values) {
      tester.step()
      tester.poke("io_a", x)
      tester.poke("io_b", y)
      tester.poke("io_e", 1)
      tester.step()

      tester.poke("io_e", 0)
      tester.step()

      while (tester.peek("io_v") != Big1) {
        tester.step()
      }

      tester.expect("io_z", z)
    }

    val stateCycles = tester.interpreter.circuitState.stateCounter

    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0

    println(
      f"processed $stateCycles cycles $elapsedSeconds%.6f seconds ${stateCycles.toDouble / (1000000.0 * elapsedSeconds)}%5.3f MHz"
    )
    tester.report()
  }

  // This test is used for benchmarking against the treadle project.
  it should "run a lot of values" in {
    manyValuesTest(24)
  }

}
