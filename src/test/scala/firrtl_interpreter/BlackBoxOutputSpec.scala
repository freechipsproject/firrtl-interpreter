// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ir.Type
import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number

/**
  * Illustrate a black box that has multiple outputs
  * This one creates 3 outputs each with a different increment of the input
  */
class FanOutAdder extends BlackBoxImplementation {
  override def name: String = "FanOutAdder"

  override def execute(inputValues: Seq[Concrete], tpe: Type, outputName: String): Concrete = {
    val inc = outputName match {
      case "out1" => 1
      case "out2" => 2
      case "out3" => 3
    }
    inputValues.head + ConcreteUInt(inc, 3)
  }

  override def cycle(): Unit = {}

  override def outputDependencies(outputName: String): Seq[String] = {
    outputName match {
      case "out1" => Seq("in")
      case "out2" => Seq("in")
      case "out3" => Seq("in")
      case _ => throw InterpreterException(s"$name was asked for input dependency for unknown output $outputName")
    }
  }
}

class FanOutAdderFactory extends BlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    Some(add(new FanOutAdder))
  }
}

class BlackBoxCounter extends BlackBoxImplementation {
  val name: String = "BlackBoxCounter"
  var counter = BigInt(0)

  def execute(inputValues: Seq[Concrete], tpe: Type, outputName: String): Concrete = {
    if(inputValues.head.value == Big1) {
      counter = 0
    }
    TypeInstanceFactory(tpe, counter)
  }

  override def cycle(): Unit = {
    counter += 1
  }

  override def outputDependencies(outputName: String): Seq[String] = {
    Seq("clear")
  }
}

class BlackBoxCounterFactory extends BlackBoxFactory {
  override def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    Some(add(new BlackBoxCounter))
  }
}

class BlackBoxOutputSpec extends FreeSpec with Matchers {
  "this tests black box implmentation that have multiple outputs" - {
    val adderInput =
      """
        |circuit FanOutTest :
        |  extmodule FanOut :
        |    output out1 : UInt<64>
        |    output out2 : UInt<64>
        |    output out3 : UInt<64>
        |    input in : UInt<64>
        |
        |
        |  module FanOutTest :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input in : UInt<64>
        |    output out1 : UInt<64>
        |    output out2 : UInt<64>
        |    output out3 : UInt<64>
        |
        |    inst fo of FanOut
        |    fo.in <= in
        |    out1 <= fo.out1
        |    out2 <= fo.out2
        |    out3 <= fo.out3
      """.stripMargin

    "each output should hold a different values" in {

      val factory = new FanOutAdderFactory

      val optionsManager = new InterpreterOptionsManager {
        interpreterOptions = InterpreterOptions(blackBoxFactories = Seq(factory), randomSeed = 0L)
      }
      val tester = new InterpretiveTester(adderInput, optionsManager)
      tester.interpreter.verbose = true
      tester.interpreter.setVerbose()

      for(i <- 0 until 10) {
        tester.poke("in", i)
        tester.expect("out1", i + 1)
        tester.expect("out2", i + 2)
        tester.expect("out3", i + 3)
        tester.step()
      }
    }
  }

  "this tests nested black box implementation that have multiple outputs" - {
    val adderInput =
      """
        |circuit FanOutTest :
        |  extmodule FanOut :
        |    output out1 : UInt<64>
        |    output out2 : UInt<64>
        |    output out3 : UInt<64>
        |    input in : UInt<64>
        |
        |  module FanOutMiddleMan :
        |    output output1 : UInt<64>
        |    output output2 : UInt<64>
        |    output output3 : UInt<64>
        |    input input1 : UInt<64>
        |
        |    inst fo of FanOut
        |    fo.in <= input1
        |    output1 <= fo.out1
        |    output2 <= fo.out2
        |    output3 <= fo.out3
        |
        |  module FanOutTest :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input in : UInt<64>
        |    output out1 : UInt<64>
        |    output out2 : UInt<64>
        |    output out3 : UInt<64>
        |
        |    inst fo of FanOutMiddleMan
        |    fo.input1 <= in
        |    out1 <= fo.output1
        |    out2 <= fo.output2
        |    out3 <= fo.output3
      """.stripMargin

    "each output should hold a different values" in {

      val factory = new FanOutAdderFactory

      val optionsManager = new InterpreterOptionsManager {
        interpreterOptions = InterpreterOptions(blackBoxFactories = Seq(factory), randomSeed = 0L)
      }
      val tester = new InterpretiveTester(adderInput, optionsManager)
      tester.interpreter.verbose = true
      tester.interpreter.setVerbose()

      for(i <- 0 until 10) {
        tester.poke("in", i)
        tester.expect("out1", i + 1)
        tester.expect("out2", i + 2)
        tester.expect("out3", i + 3)
        tester.step()
      }
    }
  }

  "this test a black box of an accumulator that implements reset" - {
    val input =
      """
        |circuit CounterTest :
        |  extmodule BlackBoxCounter :
        |    output counter : UInt<64>
        |    input clear : UInt<1>
        |
        |
        |  module CounterTest :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input clear1 : UInt<64>
        |    output counter : UInt<64>
        |
        |    inst bbc of BlackBoxCounter
        |    bbc.clear <= clear1
        |    counter <= bbc.counter
      """.stripMargin

    "counter holds internal state that is retrieved by parent" in {

      val factory = new BlackBoxCounterFactory

      val optionsManager = new InterpreterOptionsManager {
        interpreterOptions = InterpreterOptions(blackBoxFactories = Seq(factory), randomSeed = 0L)
      }
      val tester = new InterpretiveTester(input, optionsManager)
      tester.interpreter.verbose = true
      tester.interpreter.setVerbose()

      tester.poke("clear1", 1)
      tester.step()
      tester.poke("clear1", 0)

      for(i <- 0 until 10) {
        tester.expect("counter", i)
        tester.step()
      }
    }
  }

  "this test a circuit with two black box accumulators that implements reset" - {
    val input =
      """
        |circuit CounterTest :
        |  extmodule BlackBoxCounter :
        |    output counter : UInt<64>
        |    input clear : UInt<1>
        |
        |  module CounterTest :
        |    input clk : Clock
        |    input reset : UInt<1>
        |    input clear1 : UInt<64>
        |    input clear2 : UInt<64>
        |    output counter1 : UInt<64>
        |    output counter2 : UInt<64>
        |
        |    inst bbc1 of BlackBoxCounter
        |    bbc1.clear <= clear1
        |    counter1 <= bbc1.counter

        |    inst bbc2 of BlackBoxCounter
        |    bbc2.clear <= clear2
        |    counter2 <= bbc2.counter
      """.stripMargin

    "each counter should hold a different value in internal state" in {

      val factory = new BlackBoxCounterFactory

      val optionsManager = new InterpreterOptionsManager {
        interpreterOptions = InterpreterOptions(blackBoxFactories = Seq(factory), randomSeed = 0L)
      }
      val tester = new InterpretiveTester(input, optionsManager)
      tester.interpreter.verbose = true
      tester.interpreter.setVerbose()

      tester.poke("clear1", 1)
      tester.step()
      tester.poke("clear1", 0)

//      tester.step(4)

      for(i <- 0 until 10) {
        tester.expect("counter1", i)
        tester.step()
      }
      tester.poke("clear2", 1)
      tester.step()
      tester.poke("clear2", 0)


      for(i <- 0 until 10) {
        tester.expect("counter1", i + 11)
        tester.expect("counter2", i)
        tester.step()
      }
    }
  }
}