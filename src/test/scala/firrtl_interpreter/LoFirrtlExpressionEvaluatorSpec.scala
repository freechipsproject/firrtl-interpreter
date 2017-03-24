// See LICENSE for license details.
package firrtl_interpreter

import firrtl.ir._
import firrtl.PrimOps._
import firrtl.passes.PassException
import firrtl_interpreter.TestUtils._
import org.scalatest.{FlatSpec, Matchers}

// scalastyle:off magic.number

class LoFirrtlExpressionEvaluatorSpec extends FlatSpec with Matchers {
  behavior of "Mux"

  it should "throw exception if condition is not (0|1).U<1> , return true and false branch correctly" in {

    val input =
      """circuit Test :
        |  module Test :
        |    input clk : Clock
        |    input a : UInt<1>
        |    output c : UInt<2>
        |    reg w : UInt<1>, clk
        |    w <= a
        |    c <= w
      """.stripMargin

    val interpreter = FirrtlTerp(input)
    val evaluator = new LoFirrtlExpressionEvaluator(interpreter.dependencyGraph, interpreter.circuitState)

    val (trueBranch, trueResult)   = (UIntLiteral(1, IntWidth(1)), ConcreteUInt(1, 1))
    val (falseBranch, falseResult) = (UIntLiteral(0, IntWidth(1)), ConcreteUInt(0, 1))

    var condition: Expression = UIntLiteral(0, IntWidth(1))
    var expression = Mux(condition, trueBranch, falseBranch, UIntType(IntWidth(1)))
    var result = evaluator.evaluate(expression)
    result should be (falseResult)

    condition = UIntLiteral(1, IntWidth(1))
    expression = Mux(condition, trueBranch, falseBranch, UIntType(IntWidth(1)))
    result = evaluator.evaluate(expression)
    result should be (trueResult)

    condition = SIntLiteral(1, IntWidth(1))
    expression = Mux(condition, trueBranch, falseBranch, UIntType(IntWidth(1)))
    intercept[InterpreterException] {
      evaluator.evaluate(expression)
    }
    condition = UIntLiteral(1, IntWidth(2))
    expression = Mux(condition, trueBranch, falseBranch, UIntType(IntWidth(1)))
    intercept[InterpreterException] {
      evaluator.evaluate(expression)
    }
  }

  behavior of "Primitive ops"

  val input: String =
    """circuit Test :
      |  module Test :
      |    input clk : Clock
      |    input a : UInt<1>
      |    output c : UInt<2>
      |    reg w : UInt<1>, clk
      |    w <= a
      |    c <= w
    """.stripMargin

  val interpreter = FirrtlTerp(input)
  val evaluator = new LoFirrtlExpressionEvaluator(interpreter.dependencyGraph, interpreter.circuitState)
  val random = util.Random

  val baseWidth = 4

  they should "return types correctly" in {
    val w1 = IntWidth(4)
    val w2 = IntWidth(4)
    val i1 = evaluator.makeUIntValue(1, w1)
    val s1 = evaluator.makeSIntValue(2, w2)
    val outWidth = IntWidth(4)
    val outWidthType = UIntType(outWidth)

    evaluator.mathPrimitive(Mul, Seq(i1, i1), outWidthType ).isInstanceOf[ConcreteUInt] should be (true)
    evaluator.mathPrimitive(Mul, Seq(i1, s1), outWidthType ).isInstanceOf[ConcreteSInt] should be (true)
    evaluator.mathPrimitive(Mul, Seq(s1, i1), outWidthType ).isInstanceOf[ConcreteSInt] should be (true)
    evaluator.mathPrimitive(Mul, Seq(s1, s1), outWidthType ).isInstanceOf[ConcreteSInt] should be (true)

  }
  they should "evaluate multiply UIntValues correctly" in {
    for(i <- 0 to 10) {
      val w1 = IntWidth(BigInt(baseWidth, random).abs)
      val w2 = IntWidth(BigInt(baseWidth, random).abs)
      val i1 = evaluator.makeUIntValue(BigInt(baseWidth-1, random).abs, w1)
      val i2 = evaluator.makeUIntValue(BigInt(baseWidth-1, random).abs, w2)
      val outWidth = IntWidth(BigInt(baseWidth, random))
      val outWidthType = UIntType(outWidth)

      val out = evaluator.mathPrimitive(Mul, Seq(i1, i2), outWidthType )

      println(s"$i1 * $i2 => $out $outWidth")
      out.value should be (i1.value * i2.value)
    }
  }
  they should "evaluate multiply SIntValues correctly" in {

    for(i <- 0 to 10) {
      val w1 = IntWidth(BigInt(baseWidth, random).abs + 1)
      val w2 = IntWidth(BigInt(baseWidth, random).abs + 1)

      val i1 = evaluator.makeSIntValue(BigInt(w1.width.toInt - 1, random), w1)
      val i2 = evaluator.makeSIntValue(BigInt(w2.width.toInt - 1, random), w2)

      val outWidth = IntWidth(BigInt(baseWidth, random))
      val outWidthType = UIntType(outWidth)

      println(f"sintvalue ${i1.value}%x * sintvalue ${i2.value}%x $i1 $i2")

      val out = evaluator.mathPrimitive(Mul, Seq(i1, i2), outWidthType )

      println(s"$i1 * $i2 => $out")
      out.value should be (i1.value * i2.value)
    }
  }

  behavior of "requiredBitsForUInt"

  it should "return the right amount" in {
    requiredBitsForUInt(BigInt("1"*29, 2)) should be (29)
    for( width <- 1 to 100) {
      val num = Big1 << (width - 1)
      val computed = requiredBitsForUInt(num)
      computed should be (width)

      val maxNum = BigInt("1"*width, 2)
      val maxComputed = requiredBitsForUInt(maxNum)
//      println(s"width $width computed $maxComputed num $maxNum")
      maxComputed should be (width)
    }
  }

  behavior of "mask operation"

  it should "return sensible values" in {
    val num: BigInt = BigInt("1080825922752")
    val bits: BigInt = 31
    val pow: BigInt = BigInt("2147483648")
    evaluator.mask(num, bits) should be(num % pow)

    var power: BigInt = 2
    for(maskSize <- 1 to 100) {
      for (samples <- 0 to 10) {
        val number = BigInt(maskSize + 10, random)
        val masked = evaluator.mask(number, maskSize)
        // println(s"mask $maskSize sample number $samples number $number " +
        //  s"power $power masked $masked calc ${number % power} ")
        masked should be(number % power)
      }
      if(maskSize > 0 ) power <<= 1
    }
  }

  behavior of "Shr"

  it should "throw assertions when parameter is less than zero" in {
    intercept[AssertionError] {
      evaluator.bitOps(Shr, Seq(UIntLiteral(1, IntWidth(3))), Seq(-1), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the right" in {
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num << shift
      val target = UIntLiteral(shiftedNum, IntWidth(width + shift))
      requiredBitsForUInt(num) should be (width)
      requiredBitsForUInt(shiftedNum) should be (width + shift)

//      println(s"width $width => num $num arg $shift, target $target result NA")
      val result = evaluator.bitOps(Shr, Seq(target), Seq(shift), UIntType(IntWidth(width)))
//      println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (num)
    }
    testShiftOp(29, 1)

    for(i <- 3 to 40) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
      }
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = SIntLiteral(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))

        val result = evaluator.bitOps(Shr, Seq(target), Seq(arg), SIntType(IntWidth(i + 1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
      for(arg <- 1 until i) {
        val num = -BigInt("1"*i, 2)
        val target = SIntLiteral(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))

        val result = evaluator.bitOps(Shr, Seq(target), Seq(arg), SIntType(IntWidth(i + 1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
    }
  }

  behavior of "Shl"

  it should "throw assertions when parameter is  > zero" in {
    intercept[AssertionError] {
      evaluator.bitOps(Shl, Seq(UIntLiteral(1, IntWidth(3))), Seq(-1), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the left" in {
    for(i <- 3 to 100) {
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = UIntLiteral(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)

        val result = evaluator.bitOps(Shl, Seq(target), Seq(arg), UIntType(IntWidth(i + arg)))
        // println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.width should be (i + arg)
      }
      for(arg <- 1 until 50) {
        val num = BigInt("1" * i, 2)
        val target = SIntLiteral(num, IntWidth(i + 1))
        val testTarget = evaluator.shiftLeft(num, arg)

        val result = evaluator.bitOps(Shl, Seq(target), Seq(arg), SIntType(IntWidth(i + arg)))
        // println(f"num $num%x arg $arg, target $target result ${result.value}%x.S<${result.width}>")
        result.value should be (testTarget)
        result.width should be (i + arg + 1)
      }
    }
  }

  behavior of "Dshl"

  it should "throw assertions when parameter is > zero" in {
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(Dshl,
        Seq(UIntLiteral(1, IntWidth(3)), SIntLiteral(-1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(Dshl,
        Seq(UIntLiteral(1, IntWidth(3)), SIntLiteral(1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the left" in {
    // This was in firrtl/Utils.scala ...
    def requiredBits(i: BigInt): Int = {
      val ix = if (i < 0) (-1 * i) - 1 else i
      ix.bitLength + 1
    }

    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num << shift
      val target = UIntLiteral(shiftedNum, IntWidth(width + shift))
      requiredBitsForUInt(num) should be (width)
      requiredBitsForUInt(shiftedNum) should be (width + shift)

//      println(s"width $width => num $num arg $shift, target $target result NA")
      val result = evaluator.bitOps(Shr, Seq(target), Seq(shift), UIntType(IntWidth(width)))
//      println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (num)
    }
    testShiftOp(29, 1)

    for(i <- 3 to 40) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
      }
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = SIntLiteral(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))

        val result = evaluator.bitOps(Shr, Seq(target), Seq(arg), SIntType(IntWidth(i + 1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
      for(arg <- 1 until i) {
        val num = -BigInt("1"*i, 2)
        val target = SIntLiteral(evaluator.shiftLeft(num, arg), IntWidth(i + arg + 1))

        val result = evaluator.bitOps(Shr, Seq(target), Seq(arg), SIntType(IntWidth(i + 1)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (num)
      }
    }
    for(i <- 3 to 100) {
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = UIntLiteral(num, IntWidth(i))
        val testTarget = evaluator.shiftLeft(num, arg)
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(requiredBits(arg)))

        val result = evaluator.dynamicBitOps(Dshl, Seq(target, shiftValue),
          Seq(), UIntType(IntWidth(i + arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.width should be (i + arg)
      }
      for(arg <- 1 until 50) {
        val num = BigInt("1"*i, 2)
        val target = SIntLiteral(num, IntWidth(i + 1))
        val testTarget = evaluator.shiftLeft(num, arg)
        val shiftValue = evaluator.makeUIntValue(arg, IntWidth(requiredBits(arg)))

        val result = evaluator.dynamicBitOps(Dshl, Seq(target, shiftValue),
          Seq(), SIntType(IntWidth(i + arg)))
        //        println(s"num $num arg $arg, target $target result $result")
        result.value should be (testTarget)
        result.width should be (i + arg + 1)
      }
    }
  }

  behavior of "Dshr"

  it should "throw assertions when parameter is zero" in {
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(Dshr,
        Seq(UIntLiteral(1, IntWidth(3)), UIntLiteral(4, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
    intercept[InterpreterException] {
      evaluator.dynamicBitOps(Dshr,
        Seq(UIntLiteral(1, IntWidth(3)), SIntLiteral(1, IntWidth(2))), Seq(), UIntType(IntWidth(3)))
    }
  }
  it should "shift bits n bits to the right" in {
    def testShiftOp(width: Int, shift: Int): Unit = {
      val num = BigInt("1"*width, 2)
      val shiftedNum = num >> shift

      val target = UIntLiteral(num, IntWidth(width))
      val shiftUInt = UIntLiteral(shift, IntWidth(requiredBitsForUInt(shift)))

      requiredBitsForUInt(num) should be (width)
      requiredBitsForUInt(shiftedNum) should be (width - shift)

      val result = evaluator.dynamicBitOps(Dshr, Seq(target, shiftUInt),
        Seq(), UIntType(IntWidth(width)))
//      println(s"width $width => num $num arg $shift, target $target result $result")
      result.value should be (shiftedNum)
    }

    for(i <- 3 to 20) {
      for(arg <- 1 until i) {
        testShiftOp(i, arg)
      }
    }
  }
  it should "not create negative numbers" in {
    val shifted = ConcreteSInt(BigInt("-13944528177324565200897"), 75)
    val shifter = ConcreteUInt(74, 7)

    val res = shifted >> shifter
    res.value should be (-1)
  }

  behavior of "Head"

  it should "throw assertions when parameter is zero or not less than width of target" in {
    intercept[AssertionError] {
      evaluator.bitOps(Head, Seq(UIntLiteral(1, IntWidth(3))), Seq(0), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(Head, Seq(UIntLiteral(1, IntWidth(3))), Seq(4), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(Head, Seq(UIntLiteral(1, IntWidth(33))), Seq(34), UIntType(IntWidth(3)))
    }
  }
  it should "shift n bits at top of number over the width - n bits to the right" in {
    for(i <- 1 to 100) {
      for(arg <- 1 until i) {
        val num = BigInt("1"*i, 2)
        val target = UIntLiteral(evaluator.shiftLeft(num, arg), IntWidth(i + arg))

//        println(s"i $i num $num arg $arg, target $target result NA")
        val result = evaluator.bitOps(Head, Seq(target), Seq(i), UIntType(IntWidth(i)))
        result.value should be (num)
      }
    }
  }

  behavior of "tail"

  it should "throw assertions when parameter is zero or not less than width of target" in {
    intercept[AssertionError] {
      evaluator.bitOps(Tail, Seq(UIntLiteral(1, IntWidth(3))), Seq(-1), UIntType(IntWidth(3)))
    }
    intercept[AssertionError] {
      evaluator.bitOps(Tail, Seq(UIntLiteral(1, IntWidth(33))), Seq(34), UIntType(IntWidth(3)))
    }
  }
  it should "remove top n bits of a number" in {
    for(i <- IntWidthTestValuesGenerator(1, TestUtils.MaxWidth)) {
      for(arg <- IntWidthTestValuesGenerator(0, i-1)) {
        val num  = allOnes(i)
        val mask = allOnes(i-arg)

        val target = UIntLiteral(num, IntWidth(i))
        val result = evaluator.bitOps(Tail, Seq(target), Seq(arg), UIntType(IntWidth(i-arg)))
//        println(s"num $num arg $arg, result $result")
        result.value should be (mask)
      }
    }
  }

  behavior of "combinational loops"

  it should "throw exception when found" in {
    // val steam = getClass.getResourceAsStream("/rocket.fir")
    val stream = getClass.getResourceAsStream("/HasLoop.fir")
    val input = io.Source.fromInputStream(stream).mkString

    intercept[PassException] {
      val interpreter = FirrtlTerp(input)
      interpreter.evaluateCircuit()
    }
  }

}
