// See LICENSE for license details.

package firrtl_interpreter

import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number
class AndrOrrXorrSpec extends FreeSpec with Matchers {
  "Xorr reduction should work with vec converted to uint" in {
    val input =
      """
        |circuit XorReduce :
        |  module XorReduce :
        |    input clock : Clock
        |    input reset : UInt<1>
        |    input io_in1_0 : UInt<1>
        |    input io_in1_1 : UInt<1>
        |    input io_in1_2 : UInt<1>
        |    input io_in1_3 : UInt<1>
        |    output io_out_andr : UInt<1>
        |    output io_out_orr : UInt<1>
        |    output io_out_xorr : UInt<1>
        |
        |    node _T = cat(io_in1_1, io_in1_0) @[XorReducer.scala 15:21]
        |    node _T_1 = cat(io_in1_3, io_in1_2) @[XorReducer.scala 15:21]
        |    node _T_2 = cat(_T_1, _T) @[XorReducer.scala 15:21]
        |
        |    node _T_3 = andr(_T_2) @[XorReducer.scala 15:28]
        |    node _T_4 = orr(_T_2) @[XorReducer.scala 15:28]
        |    node _T_5 = xorr(_T_2) @[XorReducer.scala 15:28]
        |    io_out_andr <= _T_3 @[XorReducer.scala 15:11]
        |    io_out_orr <= _T_4 @[XorReducer.scala 15:11]
        |    io_out_xorr <= _T_5 @[XorReducer.scala 15:11]
      """.stripMargin

    def scalaXorReduce(x: BigInt, width: Int): Int = {
      if(x.bitCount % 2 == 0) 0 else 1
    }

    def scalaAndReduce(x: BigInt, width: Int): Int = {
      if((0 until width).forall(i => x.testBit(i))) 1 else 0
    }

    def scalaOrReduce(x: BigInt, width: Int): Int = {
      if((0 until width).exists(i => x.testBit(i))) 1 else 0
    }

    val t = new InterpretiveTester(input)
    for {
      i0 <- 0 to 1
      i1 <- 0 to 1
      i2 <- 0 to 1
      i3 <- 0 to 1
    } {
      t.poke(s"io_in1_0", i0)
      t.poke(s"io_in1_1", i1)
      t.poke(s"io_in1_2", i2)
      t.poke(s"io_in1_3", i3)

      t.expect("io_out_andr", scalaAndReduce(i0 + (i1 << 1) + (i2 << 2) + (i3 << 3), 4))
      t.expect("io_out_orr", scalaOrReduce(i0 + (i1 << 1) + (i2 << 2) + (i3 << 3), 4))
      t.expect("io_out_xorr", scalaXorReduce(i0 + (i1 << 1) + (i2 << 2) + (i3 << 3), 4))

//      println(s"got $i0$i1$i2$i3 " + t.peek("io_out_andr") + " " +
//              t.peek("io_out_orr") + " " + t.peek("io_out_xorr"))
    }

    t.report()
  }
}
