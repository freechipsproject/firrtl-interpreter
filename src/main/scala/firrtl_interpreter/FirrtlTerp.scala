// See LICENSE for license details.

package firrtl_interpreter

import firrtl_interpreter.real.DspRealFactory
import firrtl.ir._

// TODO: consider adding x-state
// TODO: Support forced values on nodes (don't recompute them if forced)
// TODO: How do zero width wires affect interpreter

/**
  * This is the Firrtl interpreter.  It is the top level control engine
  * that controls the simulation of a circuit running.
  *
  * It coordinates updating of the circuit's inputs (other elements, nodes,
  * registers, etc can be forced to values) and querying the circuits outputs
  * (or optionally other circuit components)
  *
  * This mainly involves updating of a circuit state instance by using a
  * expression evaluator on a dependency graph.
  *
  * @param ast the circuit to be simulated
  */
class FirrtlTerp(val ast: Circuit, val interpreterOptions: InterpreterOptions) extends SimpleLogger {
  var lastStopResult: Option[Int] = None
  def stopped: Boolean = lastStopResult.nonEmpty
  def stopResult: Int  = lastStopResult.get

  val loweredAst: Circuit = if(interpreterOptions.lowCompileAtLoad) ToLoFirrtl.lower(ast) else ast

  if(interpreterOptions.showFirrtlAtLoad) {
    println("LoFirrtl" + "=" * 120)
    println(loweredAst.serialize)
  }

  val blackBoxFactories: Seq[BlackBoxFactory] = interpreterOptions.blackBoxFactories

  /**
    * turns on evaluator debugging. Can make output quite
    * verbose.
    *
    * @param value  The desired verbose setting
    */
  override def setVerbose(value: Boolean): Unit = {
    super.setVerbose(value)
    evaluator.setVerbose(value)
  }

  val dependencyGraph = DependencyGraph(loweredAst, this)
  /**
    * Once a stop has occured, the intepreter will not allow pokes until
    * the stop has been cleared
    */
  def clearStop(): Unit = {lastStopResult = None}

  var circuitState = CircuitState(dependencyGraph)
  println("Circuit state created")

  def makeVCDLogger(fileName: String, showUnderscored: Boolean): Unit = {
    circuitState.makeVCDLogger(dependencyGraph, circuitState, fileName, showUnderscored)
  }
  def disableVCD(): Unit = {
    circuitState.disableVCD()
  }
  def writeVCD(): Unit = {
    circuitState.writeVCD()
  }

  val evaluator = new LoFirrtlExpressionEvaluator(
    dependencyGraph = dependencyGraph,
    circuitState = circuitState
  )
  evaluator.evaluationStack.maxExecutionDepth = interpreterOptions.maxExecutionDepth
  val timer: Timer = evaluator.timer

  setVerbose(interpreterOptions.setVerbose)

  def getValue(name: String): Concrete = {
    assert(dependencyGraph.validNames.contains(name),
      s"Error: getValue($name) is not an element of this circuit")

    if(circuitState.isStale) {
        evaluateCircuit()
      }
    circuitState.getValue(name) match {
      case Some(value) => value
      case _ => throw InterpreterException(s"Error: getValue($name) returns value not found")
    }
  }

  def getSpecifiedValue(name: String): Concrete = {
    assert(dependencyGraph.validNames.contains(name),
      s"Error: getValue($name) is not an element of this circuit")

    if(circuitState.isStale) {
      evaluateCircuit(Seq(name))
    }
    circuitState.getValue(name) match {
      case Some(value) => value
      case _ => throw InterpreterException(s"Error: getValue($name) returns value not found")
    }
  }

  def setValue(name: String, value: Concrete, force: Boolean = true, registerPoke: Boolean = false): Concrete = {
    if(!force) {
      assert(circuitState.isInput(name),
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
      if(checkStopped("setValue")) return Concrete.poisonedUInt(1)
    }

    circuitState.setValue(name, value, registerPoke = registerPoke)
  }

  /**
    * Creates a concrete based on current circuit and the value and poisoned state
    * It uses the type of any existing value for name and if it can't find that it
    * looks up the type in the dependency graph
    * this handles setting SInts with negative values, from positive bigInts when sized appropriately
    * @param name  name of value to set
    * @param value new value
    * @return the concrete value that was derived from type and value
    */
  def makeConcreteValue(name: String, value: BigInt, poisoned: Boolean = false): Concrete = {
    circuitState.getValue(name) match {
      case Some(currentValue) =>
        TypeInstanceFactory.makeSimilar(currentValue, value, poisoned = poisoned)
      case None =>
        TypeInstanceFactory(dependencyGraph.nameToType(name), value, poisoned = poisoned)
    }
  }

  /**
    * Update the circuit state with the supplied information
    * @param name  name of value to set
    * @param value new value
    * @param force allows setting components other than top level inputs
    * @param registerPoke changes which side of a register is poked
    * @return the concrete value that was derived from type and value
    */
  def setValueWithBigInt(
      name: String, value: BigInt, force: Boolean = true, registerPoke: Boolean = false): Concrete = {

    if(!force) {
      assert(circuitState.isInput(name),
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
    }

    val concreteValue = makeConcreteValue(name, value)

    circuitState.setValue(name, concreteValue, registerPoke = registerPoke)
  }

  def hasInput(name: String): Boolean  = dependencyGraph.hasInput(name)
  def hasOutput(name: String): Boolean = dependencyGraph.hasOutput(name)

  def evaluateCircuit(specificDependencies: Seq[String] = Seq()): Unit = {
    log(s"clear ephemera")
    circuitState.prepareForDependencyResolution()
    log(circuitState.prettyString())
    log(s"resolve dependencies")
    evaluator.resolveDependencies(specificDependencies)

    if(specificDependencies.isEmpty) {
      circuitState.isStale = false
    }
  }

  def reEvaluate(name: String): Unit = {
    setVerbose()
    evaluateCircuit(Seq(name))
  }

  def checkStopped(attemptedCommand: String = "command"): Boolean = {
    if(stopped) {
      log(s"circuit has been stopped: ignoring $attemptedCommand")
    }
    stopped
  }

  def cycle(showState: Boolean = false): Unit = {
    if(checkStopped("cycle")) return

    circuitState.vcdRaiseClock()

    if(circuitState.isStale) {
      log("interpreter cycle() called, state is stale, re-evaluate Circuit")
      log(circuitState.prettyString())

      log(s"process reset")
      evaluateCircuit()
    }
    else {
      log(s"interpreter cycle() called, state is fresh")
    }

    circuitState.cycle()

    for (elem <- blackBoxFactories) {
      elem.cycle()
    }

//    println(s"FirrtlTerp: cycle complete ${"="*80}\n${sourceState.prettyString()}")
    log(s"check prints")
    evaluator.checkPrints()
    log(s"check stops")
    lastStopResult = evaluator.checkStops()

    log(s"${circuitState.prettyString()}")
    if(stopped) {
      if(stopResult == 0) {
        throw StopException(s"Success: Stop result $stopResult")
      }
      else {
        throw StopException(s"Failure: Stop result $stopResult")
      }
    }

    //    println(s"FirrtlTerp: cycle complete ${"="*80}\n${sourceState.prettyString()}")
    if(showState) println(s"FirrtlTerp: next state computed ${"="*80}\n${circuitState.prettyString()}")

    circuitState.vcdLowerClock()
  }

  def doCycles(n: Int): Unit = {
    if(checkStopped(s"doCycles($n)")) return

    println(s"Initial state ${"-"*80}\n${circuitState.prettyString()}")

    for(cycle_number <- 1 to n) {
      println(s"Cycle $cycle_number ${"-"*80}")
      cycle()
      if(stopped) return
    }
  }
}

object FirrtlTerp {
  val blackBoxFactory = new DspRealFactory

  def apply(input: String, interpreterOptions: InterpreterOptions = InterpreterOptions()): FirrtlTerp = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val interpreter = new FirrtlTerp(ast, interpreterOptions)

    /* run the circuit once to get the circuit state fully populated. Evaluate all makes sure both
    branches of muxes get computed, while we are at we can compute the sort key order
     */
    try {
      val saveUseTopologicalSortedKeys = interpreter.evaluator.useTopologicalSortedKeys
      val saveEvaluateAll = interpreter.evaluator.evaluateAll

      interpreter.evaluator.evaluateAll = true
      interpreter.evaluator.useTopologicalSortedKeys = true
      interpreter.evaluateCircuit()

      interpreter.evaluator.useTopologicalSortedKeys = saveUseTopologicalSortedKeys
      interpreter.evaluator.evaluateAll = saveEvaluateAll
    }
    catch {
      case ie: InterpreterException =>
        println(s"Error: InterpreterExecption(${ie.getMessage} during warmup evaluation")
    }
    interpreter
  }

  @deprecated("This loses options passed in from the command line, use other appluy methods", since = "2017-01-12")
  def apply(input: String,
            verbose: Boolean,
            blackBoxFactories: Seq[BlackBoxFactory]): FirrtlTerp = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val options = InterpreterOptions(blackBoxFactories = blackBoxFactories, setVerbose = verbose)
    val interpreter = new FirrtlTerp(ast, options)

    /* run the circuit once to get the circuit state fully populated. Evaluate all makes sure both
    branches of muxes get computed, while we are at we can compute the sort key order
     */
    try {
      val saveUseTopologicalSortedKeys = interpreter.evaluator.useTopologicalSortedKeys
      val saveEvaluateAll = interpreter.evaluator.evaluateAll

      interpreter.evaluator.evaluateAll = true
      interpreter.evaluator.useTopologicalSortedKeys = true
      interpreter.evaluateCircuit()

      interpreter.evaluator.useTopologicalSortedKeys = saveUseTopologicalSortedKeys
      interpreter.evaluator.evaluateAll = saveEvaluateAll
    }
    catch {
      case ie: InterpreterException =>
        println(s"Error: InterpreterExecption(${ie.getMessage} during warmup evaluation")
    }
    interpreter
  }

  def apply(ast: Circuit, blackBoxFactories: Seq[BlackBoxFactory]): FirrtlTerp = {
    apply(ast, blackBoxFactories = blackBoxFactories)
  }
}
