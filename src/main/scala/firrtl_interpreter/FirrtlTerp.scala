// See LICENSE for license details.

package firrtl_interpreter

import firrtl._

// TODO: Add poison concept/multi-state
// TODO: try inlining pass
// TODO: Implement VCD parser and emitter (https://github.com/impedimentToProgress/ProcessVCD.git)?
// TODO: Support forced values on nodes (don't recompute them if forced)
// TODO: How do zero width wires affect interpreter
// TODO: Figure out what to do about clock
// TODO: Check for loops in dependency graph during evaluation
// TODO: Get official Firrtl to LoFirrtl transformer

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
class FirrtlTerp(ast: Circuit) extends SimpleLogger {
  var lastStopResult: Option[Int] = None
  def stopped: Boolean = lastStopResult.nonEmpty
  def stopResult: Int  = lastStopResult.get

  val loweredAst = ToLoFirrtl.lower(ast)
  println("LoFirrtl" + "="*120)
  println(loweredAst.serialize)

  override def setVerbose(value: Boolean): Unit = {
    super.setVerbose(value)
    evaluator.setVerbose(value)
  }

  val dependencyGraph    = DependencyGraph(loweredAst)

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
  val timer = evaluator.timer
  println("evaluator created")

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
    }

    circuitState.setValue(name, value)
  }

  def setValueWithBigInt(name: String, value: BigInt, force: Boolean = true): Concrete = {
    if(!force) {
      assert(circuitState.isInput(name),
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
    }
    val concreteValue = TypeInstanceFactory(dependencyGraph.nameToType(name), value)

    circuitState.setValue(name, concreteValue)
  }

  def hasInput(name: String): Boolean  = dependencyGraph.hasInput(name)
  def hasOutput(name: String): Boolean = dependencyGraph.hasOutput(name)

  def evaluateCircuit(specificDependencies: Seq[String] = Seq()): Unit = {
    log(s"clear ephemera")
    circuitState.prepareForDependencyResolution()
    log(circuitState.prettyString())
    log(s"resolve dependencies")
    evaluator.resolveDependencies(specificDependencies)
    log(s"process reset")
    evaluator.processRegisterResets()
    log(s"check prints")
    evaluator.checkPrints()
    log(s"check stops")
    lastStopResult = evaluator.checkStops()
    circuitState.isStale = false
    log(s"${circuitState.prettyString()}")
  }

  def reEvaluate(name: String): Unit = {
    setVerbose(true)
    evaluateCircuit(Seq(name))
  }

  def cycle(showState: Boolean = false): Unit = {
    if(circuitState.isStale) {
      log("interpreter cycle() called, state is stale, re-evaluate Circuit")
      log(circuitState.prettyString())
      evaluateCircuit()
    }
    else {
      log(s"interpreter cycle() called, state is fresh")
    }

    circuitState.cycle()

//    println(s"FirrtlTerp: cycle complete ${"="*80}\n${sourceState.prettyString()}")
    if(showState) println(s"FirrtlTerp: next state computed ${"="*80}\n${circuitState.prettyString()}")

  }

  def doCycles(n: Int): Unit = {
    println(s"Initial state ${"-"*80}\n${circuitState.prettyString()}")

    for(cycle_number <- 1 to n) {
      println(s"Cycle $cycle_number ${"-"*80}")
      cycle()
      if(stopped) return
    }
  }
}

object FirrtlTerp {
  def apply(input: String, verbose: Boolean = false): FirrtlTerp = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val interpreter = new FirrtlTerp(ast)
    interpreter.setVerbose(verbose)
    interpreter.evaluateCircuit()
    interpreter
  }
}
