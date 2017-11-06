// See LICENSE for license details.

package firrtl_interpreter

import firrtl.PortKind
import firrtl.ir.Circuit
import firrtl_interpreter.executable._
import firrtl_interpreter.real.DspRealFactory

//scalastyle:off magic.number
class FirrtlTerp(val ast: Circuit, val optionsManager: HasInterpreterSuite) {
  val interpreterOptions: InterpreterOptions = optionsManager.interpreterOptions

  var lastStopResult: Option[Int] = None
  def stopped: Boolean = lastStopResult.nonEmpty
  var verbose: Boolean = false

  val loweredAst: Circuit = if(interpreterOptions.lowCompileAtLoad) {
    ToLoFirrtl.lower(ast, optionsManager)
  } else {
    ast
  }

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
  def setVerbose(value: Boolean = true): Unit = {
    // Logger.setLevel(classOf[FirrtlTerp], LogLevel.None)
    //TODO: This is supposed to set verbose execution
  }

  val timer = new Timer

  val symbolTable: SymbolTable = timer("build symbol table") {
    SymbolTable(loweredAst, blackBoxFactories)
  }

  val dataStore = DataStore(numberOfBuffers = 1)
  symbolTable.allocateData(dataStore)
  println(s"Symbol table:\n${symbolTable.render}")

  val scheduler = new Scheduler(dataStore, symbolTable)
  val program = Program(symbolTable, dataStore, scheduler)

  val compiler = new ExpressionCompiler(program, this)

  timer("compile") {
    compiler.compile(loweredAst, blackBoxFactories)
  }

  // println(s"Scheduler before sort ${scheduler.renderHeader}")
  scheduler.sortCombinationalAssigns()
  scheduler.sortTriggeredAssigns()

  val clockOption: Option[Symbol] = {
    symbolTable.get("clock") match {
      case Some(clock) => Some(clock)
      case _           => symbolTable.get("clk")
    }
  }

  println(s"Scheduler after sort ${scheduler.render}")

  /**
    * Once a stop has occured, the interpreter will not allow pokes until
    * the stop has been cleared
    */
  def clearStop(): Unit = {lastStopResult = None}

  def makeVCDLogger(fileName: String, showUnderscored: Boolean): Unit = {
    //TODO: (chick) circuitState.makeVCDLogger(dependencyGraph, circuitState, fileName, showUnderscored)
  }
  def disableVCD(): Unit = {
    //TODO: (chick) circuitState.disableVCD()
  }
  def writeVCD(): Unit = {
    //TODO: (chick) circuitState.writeVCD()
  }



  setVerbose(interpreterOptions.setVerbose)

  var isStale: Boolean = false

  def getValue(name: String): BigInt = {
    assert(symbolTable.contains(name),
      s"Error: getValue($name) is not an element of this circuit")

    if(isStale) scheduler.makeFresh()

    val symbol = symbolTable(name)
    dataStore(symbol)
  }

  /**
    * This function used to show the calculation of all dependencies resolved to get value
    * @param name signal to get and show computation
    * @return
    */
  def getSpecifiedValue(name: String): BigInt = {
    //TODO: (chick) Show this in some other way
    assert(symbolTable.contains(name),
      s"Error: getValue($name) is not an element of this circuit")

    if(isStale) scheduler.makeFresh()

    dataStore(symbolTable(name))
  }

  /**
    * Update the circuit state with the supplied information
    * @param name  name of value to set
    * @param value new concrete value
    * @param force allows setting components other than top level inputs
    * @param registerPoke changes which side of a register is poked
    * @return the concrete value that was derived from type and value
    */
  def setValue(name: String, value: BigInt, force: Boolean = true, registerPoke: Boolean = false): BigInt = {
    assert(symbolTable.contains(name))
    val symbol = symbolTable(name)

    if(!force) {
      assert(symbol.dataKind == PortKind,
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
      if(checkStopped("setValue")) return Big0
    }

    dataStore(symbol) = value
    value
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
      name: String, value: BigInt, force: Boolean = true, registerPoke: Boolean = false): BigInt = {
    assert(symbolTable.contains(name))
    val symbol = symbolTable(name)

    if(!force) {
      assert(symbol.dataKind == PortKind,
        s"Error: setValue($name) not on input, use setValue($name, force=true) to override")
      if(checkStopped("setValue")) return Big0
    }

    dataStore(symbol) = value
    value
  }

  def isRegister(name: String): Boolean = {
    symbolTable.registerNames.contains(name)
  }

  def getRegisterNames: Seq[String] = {
    symbolTable.registerNames.toSeq
  }

  def getInputPorts: Seq[String] = {
    symbolTable.inputPortsNames.toSeq
  }

  def getOutputPorts: Seq[String] = {
    symbolTable.outputPortsNames.toSeq
  }

  def isInputPort(name: String): Boolean = {
    symbolTable.inputPortsNames.contains(name)
  }

  def isOutputPort(name: String): Boolean = {
    symbolTable.outputPortsNames.contains(name)
  }

  def validNames: Iterable[String] = symbolTable.keys
  def symbols: Iterable[Symbol] = symbolTable.symbols

  def evaluateCircuit(specificDependencies: Seq[String] = Seq()): Unit = {
    program.dataStore.advanceBuffers()
    program.scheduler.executeCombinational()
    program.scheduler.getTriggerExpressions.foreach { key =>
      program.scheduler.executeTriggeredAssigns(key)
    }
    program.scheduler.executeCombinational()
//    println(s"c --  ${program.dataInColumns}")    program.dataStore.advanceBuffers()
//    println(s"h --  ${program.header}")
//    program.scheduler.executeCombinational()
//    println(s"c --  ${program.dataInColumns}")
//    program.scheduler.getTriggerExpressions.foreach { key => program.scheduler.executeTriggeredAssigns(key) }
//    println(s"r --  ${program.dataInColumns}")
//    program.scheduler.executeCombinational()
//    println(s"c --  ${program.dataInColumns}")
  }

  def reEvaluate(name: String): Unit = {
    setVerbose()
    evaluateCircuit(Seq(name))
  }

  def checkStopped(attemptedCommand: => String = "command"): Boolean = {
    if(stopped) {
    }
    stopped
  }

  def cycle(showState: Boolean = false): Unit = {
    //TODO: (chick) VCD stuff is missing from here
    if(checkStopped("cycle")) return

    if(isStale) {

//      evaluateCircuit()
    }
    else {
    }

    clockOption.foreach { clock => dataStore.AssignInt(clock, GetIntConstant(1).apply).run() }
    evaluateCircuit()
    clockOption.foreach { clock => dataStore.AssignInt(clock, GetIntConstant(0).apply).run() }

    for (elem <- blackBoxFactories) {
      elem.cycle()
    }


    if(showState) println(s"FirrtlTerp: next state computed ${"="*80}\n${program.dataInColumns}")
  }

  def doCycles(n: Int): Unit = {
    if(checkStopped(s"doCycles($n)")) return

    println(s"Initial state ${"-"*80}\n${program.dataInColumns}")

    for(cycle_number <- 1 to n) {
      println(s"Cycle $cycle_number ${"-"*80}")
      cycle()
      if(stopped) return
    }
  }

  def poke(name: String, value: Int): Unit = {
    val symbol = program.symbolTable(name)
    program.dataStore(symbol) = value
  }
  def peek(name: String): Big = {
    val symbol = program.symbolTable(name)
    program.dataStore(symbol)
  }

  def getInfoString: String = "Info"  //TODO (chick) flesh this out
  def getPrettyString: String = program.dataInColumns
}

object FirrtlTerp {
  val blackBoxFactory = new DspRealFactory

  def apply(input: String, optionsManager: HasInterpreterSuite = new InterpreterOptionsManager): FirrtlTerp = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val interpreter = new FirrtlTerp(ast, optionsManager)

    /* run the circuit once to get the circuit state fully populated. Evaluate all makes sure both
    branches of muxes get computed, while we are at we can compute the sort key order
     */
    try {
//      val saveUseTopologicalSortedKeys = interpreter.evaluator.useTopologicalSortedKeys
//      val saveEvaluateAll = interpreter.evaluator.evaluateAll
//
//      interpreter.evaluator.evaluateAll = true
//      interpreter.evaluator.useTopologicalSortedKeys = true
//      interpreter.evaluateCircuit()
//
//      interpreter.evaluator.useTopologicalSortedKeys = saveUseTopologicalSortedKeys
//      interpreter.evaluator.evaluateAll = saveEvaluateAll
    }
    catch {
      case ie: InterpreterException =>
        println(s"Error: InterpreterExecption(${ie.getMessage} during warmup evaluation")
    }
    interpreter
  }
}
