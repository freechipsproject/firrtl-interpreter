// See LICENSE for license details.

package firrtl_interpreter

import firrtl_interpreter.real.DspRealFactory
import firrtl.ir._

// TODO: consider adding x-state
// TODO: Support forced values on nodes (don't recompute them if forced)
// TODO: How do zero width wires affect interpreter
// TODO: Figure out what to do about clock

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

  val loweredAst = if(interpreterOptions.lowCompileAtLoad) ToLoFirrtl.lower(ast) else ast

  if(interpreterOptions.showFirrtlAtLoad) {
    println("LoFirrtl" + "=" * 120)
    println(loweredAst.serialize)
  }

  val blackBoxFactories = interpreterOptions.blackBoxFactories

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

  def makeVCDLogger(fileName: String): Unit = {
    circuitState.makeVCDLogger(dependencyGraph, fileName)
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
  val timer = evaluator.timer

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

  def setValue(name: String, value: Concrete, force: Boolean = true): Concrete = {
    if(!force) {
      assert(circuitState.isInput(name),
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
      if(checkStopped("setValue")) return Concrete.poisonedUInt(1)
    }

    circuitState.setValue(name, value)
  }

  def setValueWithBigInt(
      name: String, value: BigInt, force: Boolean = true, registerPoke: Boolean = false): Concrete = {
    if(!force) {
      assert(circuitState.isInput(name),
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
    }
    val concreteValue = TypeInstanceFactory(dependencyGraph.nameToType(name), value)

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

    circuitState.isStale = false
  }

  def reEvaluate(name: String): Unit = {
    setVerbose(true)
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

    if(circuitState.isStale) {
      log("interpreter cycle() called, state is stale, re-evaluate Circuit")
      log(circuitState.prettyString())

      log(s"process reset")
      evaluateCircuit()
      evaluator.processRegisterResets()
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

  def apply(input: String,
            verbose: Boolean = false,
            blackBoxFactories: Seq[BlackBoxFactory] = Seq(blackBoxFactory)): FirrtlTerp = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val options = new InterpreterOptions(blackBoxFactories = blackBoxFactories, setVerbose = verbose)
    val interpreter = new FirrtlTerp(ast, options)

    try {
      interpreter.evaluateCircuit()
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
