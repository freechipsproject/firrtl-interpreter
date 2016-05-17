/*
Copyright (c) 2014 - 2016 The Regents of the University of
California (Regents). All Rights Reserved.  Redistribution and use in
source and binary forms, with or without modification, are permitted
provided that the following conditions are met:
   * Redistributions of source code must retain the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer.
   * Redistributions in binary form must reproduce the above
     copyright notice, this list of conditions and the following
     two paragraphs of disclaimer in the documentation and/or other materials
     provided with the distribution.
   * Neither the name of the Regents nor the names of its contributors
     may be used to endorse or promote products derived from this
     software without specific prior written permission.
IN NO EVENT SHALL REGENTS BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,
SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS,
ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF
REGENTS HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
REGENTS SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE. THE SOFTWARE AND ACCOMPANYING DOCUMENTATION, IF
ANY, PROVIDED HEREUNDER IS PROVIDED "AS IS". REGENTS HAS NO OBLIGATION
TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR
MODIFICATIONS.
*/
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
  */
class InterpretiveTester(input: String) {
  var expectationsMet = 0

  val interpreter = FirrtlTerp(input)

  def setVerbose(value: Boolean = true): Unit = {
    interpreter.setVerbose(value)
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
    println(
      s"test ${interpreter.loweredAst.modules.head.name} " +
        s"Success: $expectationsMet tests passed " +
        s"in ${interpreter.circuitState.stateCounter} cycles")
  }
}
