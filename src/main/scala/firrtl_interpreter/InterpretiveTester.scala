// See LICENSE for license details.
package firrtl_interpreter

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
  * @param input a firrtl program contained in a string
  * @param vcdOutputFileName name of file to put vcd output in, empty string turns this off
  */
class InterpretiveTester(input: String, vcdOutputFileName: String = "") {
  var expectationsMet = 0

  val interpreter = FirrtlTerp(input)
  if(vcdOutputFileName.nonEmpty) {
    interpreter.makeVCDLogger(vcdOutputFileName)
  }
  def writeVCD(): Unit = {
    interpreter.writeVCD()
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
    try {
      interpreter.setValueWithBigInt(name, value)
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
    interpreter.getValue(name) match {
      case ConcreteUInt(value, _) => value
      case ConcreteSInt(value, _) => value
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
    def testValue(value: BigInt): Unit = {
      if (value != expectedValue) {
        if(! interpreter.verbose) interpreter.reEvaluate(name)
        throw new InterpreterException (s"Error:expect($name, $expectedValue) got $value")
      }
    }
    interpreter.getValue(name) match {
      case ConcreteUInt (value, _) => testValue(value)
      case ConcreteSInt(value, _)  => testValue(value)
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
    for(_ <- 0 until n) {
      interpreter.cycle()
    }
  }

  /**
    * A simplistic report of the number of expects that passed and
    */
  def report(): Unit = {
    val endTime = System.nanoTime()
    val elapsedSeconds = (endTime - startTime).toDouble / 1000000000.0
    println(
      s"test ${interpreter.loweredAst.modules.head.name} " +
        s"Success: $expectationsMet tests passed " +
        s"in ${interpreter.circuitState.stateCounter} cycles " +
        f"taking $elapsedSeconds%.6f seconds"
    )
  }
}
