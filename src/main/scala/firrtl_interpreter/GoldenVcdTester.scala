// See LICENSE for license details.

package firrtl_interpreter

import java.io.File

import firrtl.ExecutionOptionsManager
import firrtl_interpreter.vcd.{Wire, VCD}
import logger.LazyLogging

class GoldenVcdTester(
    optionsManager: ExecutionOptionsManager with HasReplConfig with HasInterpreterOptions)
  extends LazyLogging {

  private def getInput(fileName: String): String = {
    var file = new File(fileName)
    if(! file.exists()) {
      file = new File(fileName + ".fir")
      if(! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    io.Source.fromFile(file).mkString
  }

  val replConfig = optionsManager.replConfig
  val interpreterOptions = optionsManager.interpreterOptions

  val tester = new InterpretiveTester(getInput(replConfig.firrtlSourceName), optionsManager)
  val interpreter = tester.interpreter

  val dutName = interpreter.ast.main

  val vcd: VCD = VCD.read(optionsManager.getVcdFileName, dutName)
  val timeStamps = vcd.valuesAtTime.keys.toList.sorted.toArray
  var runVerbose = false

  val vcdCircuitState = interpreter.circuitState.clone
  val inputs = {
    vcd.scopeRoot.wires
      .filter { wire =>
        interpreter.circuitState.isInput(wire.name)
      }
      .map(_.name).toSet
  }

  def setValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName
    if (interpreter.circuitState.nameToConcreteValue.contains(fullName)) {
      val bigIntValue = if(newValue < BigInt(0)) BigInt(1) else newValue
      val newConcreteValue = interpreter.makeConcreteValue(fullName, bigIntValue, poisoned = bigIntValue < BigInt(0))
      val isRegister = interpreter.circuitState.registers.contains(fullName)
      interpreter.setValue(fullName, newConcreteValue, registerPoke = isRegister)
      println(s"$fullName <= ${newConcreteValue.showValue}")
    }
  }

  def checkValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName

    if (interpreter.circuitState.nameToConcreteValue.contains(fullName)) {
      val circuitValue = interpreter.getValue(fullName)
      val vcdValue = TypeInstanceFactory.makeSimilar(circuitValue, newValue, poisoned = false)

      val result = (circuitValue.poisoned, vcdValue.poisoned) match {
        case (true, true) => "ok"
        case (false, false) =>
          if(circuitValue.value == vcdValue.value) "ok" else Console.RED + "bad" + Console.RESET
        case (_, _) => Console.RED + "bad" + Console.RESET
      }
      println(s"Testing $fullName: circuit $circuitValue, vcd $vcdValue $result")
    }
  }

  def setInitialValues(): Unit = {
    vcd.initialValues.foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        setValue(wire, change.value)
      }
    }
  }

  def setInputs(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        val fullName = change.wire.fullName
        if (inputs.contains(fullName) && interpreter.circuitState.isInput(fullName)) {
          setValue(wire, change.value)
        }
      }
    }
  }

  def testWires(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        val fullName = change.wire.fullName
        if ( !(inputs.contains(fullName) && interpreter.circuitState.isInput(fullName))) {
          checkValue(wire, change.value)
        }
      }
    }
  }

  def checkClock(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      vcd.wiresFor(change).foreach { wire =>
        val fullName = change.wire.fullName
        if(fullName == "clock" && change.value > BigInt(0)) {
          interpreter.cycle()
        }
      }
    }
  }

  def run(): Unit = {
    println(s"Initial values:\n${vcd.initialValues.mkString("\n")}")
    setInitialValues()

    for(timeIndex <- timeStamps.indices) {
      println(s"Time[$timeIndex]: ${timeStamps(timeIndex)}")

      if(runVerbose) println(s"${vcd.valuesAtTime(timeStamps(timeIndex)).mkString("\n")}")

      checkClock(timeIndex)
      setInputs(timeIndex)
      testWires(timeIndex)
      if(runVerbose) println(interpreter.circuitState.prettyString())
    }
  }

}

object GoldenVcdTester {
  def main(args: Array[String]) {
    val optionsManager = new ExecutionOptionsManager("firrtl-repl") with HasReplConfig with HasInterpreterOptions

    optionsManager.parse(args) match {
      case true =>
        val repl = new GoldenVcdTester(optionsManager)
        repl.run()
      case _ =>
    }
  }
}
