// See LICENSE for license details.

package firrtl_interpreter

import java.io.File

import firrtl.{AnnotationSeq, HasFirrtlExecutionOptions}
import firrtl.options.{DriverExecutionResult, ExecutionOptionsManager}
import firrtl_interpreter.vcd.{VCD, Wire}
import logger.LazyLogging

import firrtl.options.Viewer._
import firrtl_interpreter.InterpreterViewer._
import firrtl_interpreter.VcdReplayOptionsViewer._

/**
  * This tester runs a VCD file against a circuit expressed in a firrtl file.  The VCD file should
  * have been produced by running a test harness against the circuit.  This test can be used to
  * generate circuit behavior while running symbolic or concolic testing.
  * It can also be used to determine if later changes to a circuit have changed since some original
  * correct **golden** run of the circuit
  * For example use the main below to run the VcdAdder files contained in the src/test/resources directory
  * {{{
  * sbt 'run-main firrtl_interpreter.VcdReplayTester -fs src/test/resources/VcdAdder.fir \
  * -vcd src/test/resources/VcdAdder.vcd'
  * }}}
  *
  * @param annotationSeq options be here
  */
class VcdReplayTester(annotationSeq: AnnotationSeq) extends LazyLogging {

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

  val vcdTesterOptions: VcdReplayExecutionOptions = view[VcdReplayExecutionOptions](annotationSeq).get
  val interpreterOptions  : InterpreterExecutionOptions   = view[InterpreterExecutionOptions](annotationSeq).get


  val tester: InterpretiveTester = new InterpretiveTester(getInput(vcdTesterOptions.firrtlSourceName), annotationSeq)
  val interpreter: FirrtlTerp = tester.interpreter

  val dutName: String = interpreter.ast.main

  val vcd: VCD = VCD.read(vcdTesterOptions.vcdSourceName, dutName)
  val timeStamps: Array[Long] = vcd.valuesAtTime.keys.toList.sorted.toArray
  var runVerbose: Boolean = false

  private var eventsRun = 0
  private var inputValuesSet = 0L
  private var valuesTested = 0L
  private var testSuccesses = 0L
  private var testFailures = 0L
  private var clockCycles = 0L

  val vcdCircuitState: CircuitState = interpreter.circuitState.clone
  val inputs: Set[String] = {
    vcd.scopeRoot.wires
      .filter { wire =>
        interpreter.circuitState.isInput(wire.name)
      }
      .map(_.name).toSet
  }

  def setValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName
    if (interpreter.circuitState.nameToConcreteValue.contains(fullName)) {
      val isPoisoned = newValue < BigInt(0)
      val bigIntValue = if(isPoisoned) BigInt(1) else newValue
      val newConcreteValue = interpreter.makeConcreteValue(fullName, bigIntValue, poisoned = isPoisoned)
      val isRegister = interpreter.circuitState.registers.contains(fullName)
      interpreter.setValue(fullName, newConcreteValue, registerPoke = isRegister)
      println(s"$fullName <= ${newConcreteValue.showValue}")
      inputValuesSet += 1
    }
  }

  def checkValue(wire: Wire, newValue: BigInt): Unit = {
    val fullName = wire.fullName

    valuesTested += 1

    if (interpreter.circuitState.nameToConcreteValue.contains(fullName)) {
      val circuitValue = interpreter.getValue(fullName)
      val isPoisoned = newValue < BigInt(0)
      val bigIntValue = if(isPoisoned) BigInt(1) else newValue
      val vcdValue = interpreter.makeConcreteValue(fullName, bigIntValue, poisoned = isPoisoned)

      val result = (circuitValue.poisoned, vcdValue.poisoned) match {
        case (true, true) =>
          testSuccesses += 1
          "ok"
        case (false, false) =>
          if(circuitValue.value == vcdValue.value) {
            testSuccesses += 1
            "ok"
          }
          else {
            testFailures += 1
            Console.RED + "bad" + Console.RESET
          }
        case (_, _) =>
          testFailures += 1
          Console.RED + "bad" + Console.RESET
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
      if (vcdTesterOptions.testAliasedWires) {
        vcd.wiresFor(change).foreach { wire =>
          val fullName = change.wire.fullName
          if (!(inputs.contains(fullName) && interpreter.circuitState.isInput(fullName))) {
            checkValue(wire, change.value)
          }
        }
      }
      else {
        val fullName = change.wire.fullName
        if (!(inputs.contains(fullName) && interpreter.circuitState.isInput(fullName))) {
          checkValue(change.wire, change.value)
        }
      }
    }
  }

  def checkClock(timeIndex: Int): Unit = {
    vcd.valuesAtTime(timeStamps(timeIndex)).foreach { change =>
      vcd.wiresFor(change).exists { _ =>
        val fullName = change.wire.fullName
        if(fullName == "clock" && change.value > BigInt(0)) {
          interpreter.cycle()
          clockCycles += 1
        }
        true
      }
    }
  }

  def run(): Unit = {
    println(s"Initial values:\n${vcd.initialValues.mkString("\n")}")
    setInitialValues()
    val start = vcdTesterOptions.skipEvents
    val end = if(vcdTesterOptions.eventsToRun > 0) start + vcdTesterOptions.eventsToRun else timeStamps.length

    val startTime = System.currentTimeMillis()
    for(timeIndex <- start until end) {
      eventsRun += 1
      println(s"Time[$timeIndex]: ${timeStamps(timeIndex)}")

      if(runVerbose) println(s"${vcd.valuesAtTime(timeStamps(timeIndex)).mkString("\n")}")

      checkClock(timeIndex)
      setInputs(timeIndex)
      testWires(timeIndex)
      if(runVerbose) println(interpreter.circuitState.prettyString())
    }
    val endTime = System.currentTimeMillis()


    println(f"events run:       $eventsRun%10d")
    println(f"input values set: $inputValuesSet%10d")
    println(f"values tested:    $valuesTested%10d")
    println(f"test successes:   $testSuccesses%10d")
    println(f"test failures:    $testFailures%10d")
    println(f"clock cycles:     $clockCycles%10d")
    println(f"                  ${clockCycles / ((endTime - startTime) / 1000.0)}%10.2f Hz")
    println(f"run time:         ${(endTime - startTime) / 1000.0}%10.2f seconds")
  }
}

case object VcdReplayExecutionResult extends DriverExecutionResult

object VcdReplayTester extends firrtl.options.Driver {
  val optionsManager: ExecutionOptionsManager = {
    new ExecutionOptionsManager("vcd-replay") with HasFirrtlExecutionOptions
  }

  override def execute(args: Array[String], initialAnnotations: AnnotationSeq = Seq.empty): DriverExecutionResult = {
    val annotations = optionsManager.parse(args, initialAnnotations)

    val replayer = new VcdReplayTester(annotations)
    replayer.run()
    ReplExecutionResult
  }
}
