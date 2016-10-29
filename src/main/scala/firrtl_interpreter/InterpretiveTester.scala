// See LICENSE for license details.
package firrtl_interpreter

import firrtl.ExecutionOptionsManager

/**
  * Works a lot like the chisel classic tester compiles a firrtl input string
  * and allows poke, peek, expect and step
  *
  * pokes invalidate the underlying circuit
  * peek, expect and step, recompute (re-validate) the circuit before executing
  *
  * Important note: port names in LoFirrtl have replaced dot notation with underscore notation
  * so that io.a.b must be referenced as io_a_b
  *
  * @param input              a firrtl program contained in a string
  * @param optionsManager     collection of options for the interpreter
  */
class InterpretiveTester(
    input: String,
    optionsManager: ExecutionOptionsManager with HasInterpreterOptions =
      new ExecutionOptionsManager("firrtl-interpreter") with HasInterpreterOptions) {
  var expectationsMet = 0

  firrtl_interpreter.random.setSeed(optionsManager.interpreterOptions.randomSeed)

  val interpreter        = FirrtlTerp(input)
  val interpreterOptions = optionsManager.interpreterOptions
  val commonOptions      = optionsManager.commonOptions

  val blackBoxFactories = optionsManager.interpreterOptions.blackBoxFactories

  interpreter.evaluator.evaluationStack.MaxExecutionDepth = interpreterOptions.maxExecutionDepth

  setVerbose(interpreterOptions.setVerbose)

  if(interpreterOptions.writeVCD) {
    optionsManager.setTopNameIfNotSet(interpreter.loweredAst.main)
    optionsManager.makeTargetDir()
    interpreter.makeVCDLogger(interpreterOptions.vcdOutputFileName(optionsManager))
  }

  def setVerbose(value: Boolean = true): Unit = {
    interpreter.setVerbose(value)
  }

  val startTime = System.nanoTime()

  /**
    * Pokes value to the port referenced by string
    * Warning: pokes to components other than input ports is currently
    * not supported but does not cause an error warning
    * This feature should be supported soon
    *
    * @param name the name of a port
    * @param value a value to put on that port
    */
  def poke(name: String, value: BigInt): Unit = {
    if(interpreter.checkStopped(s"poke($name, $value)")) return

    try {
      interpreter.setValueWithBigInt(name, value)
    }
    catch {
      case ie: InterpreterException =>
        println(s"Error: poke($name, $value)")
        throw ie
    }
  }
  /**
    * Pokes value to the port referenced by string
    * Warning: pokes to components other than input ports is currently
    * not supported but does not cause an error warning
    * This feature should be supported soon
    *
    * @param name the name of a port
    * @param value a value to put on that port
    */
  def poke(name: String, value: Concrete): Unit = {
    if(interpreter.checkStopped(s"poke($name, $value)")) return

    try {
      interpreter.circuitState.setValue(name, value)
    }
    catch {
      case ie: InterpreterException =>
        println(s"Error: poke($name, $value)")
        throw ie
    }
  }

  /** inspect a value of a named circuit component
    *
    * @param name the name of a circuit component
    * @return A BigInt value currently set at name
    */
  def peek(name: String): BigInt = {
    if(interpreter.checkStopped(s"peek($name)")) return 0

    interpreter.getValue(name) match {
      case ConcreteUInt(value, _, _) => value
      case ConcreteSInt(value, _, _) => value
      case _ => throw new InterpreterException(s"Error:peek($name) value not found")
    }
  }

  /** inspect a value of a named circuit component
    *
    * @param name the name of a circuit component
    * @return An internal concrete value currently set at name
    */
  def peekConcrete(name: String): Concrete = {
    if(interpreter.checkStopped(s"peekConcrete($name)")) return Concrete.poisonedUInt(1)

    interpreter.getValue(name) match {
      case c: Concrete => c
      case _ => throw new InterpreterException(s"Error:peek($name) value not found")
    }
  }

  /**
    * require that a value be present on the named component
 *
    * @param name component name
    * @param expectedValue the BigInt value required
    */
  def expect(name: String, expectedValue: BigInt): Unit = {
    def testValue(concrete: Concrete): Unit = {
      if (concrete.value != expectedValue) {
        if(! interpreter.verbose) interpreter.reEvaluate(name)
        throw new InterpreterException (s"Error:expect($name, $expectedValue) got ${concrete.showValue}")
      }
    }
    if(interpreter.checkStopped(s"expect($name, $expectedValue)")) return

    interpreter.getValue(name) match {
      case value: ConcreteUInt  => testValue(value)
      case value: ConcreteSInt  => testValue(value)
      case value: ConcreteClock => testValue(value)
      case _ =>
        throw new InterpreterException(s"Error:expect($name, $expectedValue) value not found")
    }
    expectationsMet += 1
  }

  /**
    * Cycles the circuit n steps (with a default of one)
    * At each step registers and memories are advanced and all other elements recomputed
 *
    * @param n cycles to perform
    */
  def step(n: Int = 1): Unit = {
    if(interpreter.checkStopped(s"step($n)")) return

    for(_ <- 0 until n) {
      interpreter.cycle()
    }
  }

  def reportString: String = {
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0
    /*
        This should not every show the Failed message because currently the interpreter
        throws an InterpreterException on Stop (but maybe that will be made optional at some point)
        Best to leave this here for now, someone might catch the exception manually and still want to
        see this report which should include the Failed in that case
      */
    def status: String = {
      interpreter.lastStopResult match {
        case Some(stopResult) =>
          s"Failed: Stop result $stopResult:"
        case _ =>
          s"Success:"
      }
    }
    s"test ${interpreter.loweredAst.modules.head.name} " +
      s"$status $expectationsMet tests passed " +
      s"in ${interpreter.circuitState.stateCounter} cycles " +
      f"taking $elapsedSeconds%.6f seconds"
  }
  /**
    * A simplistic report of the number of expects that passed and
    */
  def report(): Unit = {
    interpreter.writeVCD()
    println(reportString)
  }
}
