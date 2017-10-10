// See LICENSE for license details.

//TODO:(chick) handle reset and multi-clock for registers
//TODO:(chick) shift amounts can only be int, this may reduce match statements

package firrtl_interpreter.executable

import firrtl._
import firrtl.ir.Circuit
import firrtl_interpreter.{BlackBoxFactory, Timer}

//scalastyle:off magic.number
class Compiler(ast: Circuit, blackBoxFactories: Seq[BlackBoxFactory]) {
  def lower(c: Circuit): Circuit = {
    val compiler = new LowFirrtlCompiler

    val annotationMap = AnnotationMap(Seq.empty)
    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, Some(annotationMap)))
    println(compileResult.emittedCircuitOption.get)
    compileResult.circuit
  }

  val loweredAst: Circuit = lower(ast)
  val timer = new Timer

  val symbolTable: SymbolTable = timer("build symbol table") {
    SymbolTable(loweredAst, Seq.empty)
  }
  val dataStore = DataStore(numberOfBuffers = 1)
  symbolTable.allocateData(dataStore)
  val scheduler = new Scheduler(dataStore, symbolTable)
  val program = new Program(symbolTable, dataStore, scheduler)

  val compiler = new ExpressionCompiler(program)

  timer("compile") {
    compiler.compile(loweredAst, blackBoxFactories)
  }

  println(s"Scheduler before sort ${scheduler.render}")
  scheduler.sortCombinationalAssigns
  println(s"Scheduler after sort ${scheduler.render}")


  println(s"symbol table size is ${symbolTable.size}, dataStore allocations ${dataStore.getSizes}")

  println(s"SymbolTable:\n${program.symbolTable.render}")

  def poke(name: String, value: Int): Unit = {
    val symbol = program.symbolTable(name)
    program.dataStore(symbol) = value
  }
  def peek(name: String): Big = {
    val symbol = program.symbolTable(name)
    program.dataStore(symbol)
  }

  def step(steps: Int = 1): Unit = {
    program.scheduler.getTriggerExpressions.foreach { key => program.scheduler.executeTriggeredAssigns(key) }
    println(s"r --  ${program.dataInColumns}")
    program.scheduler.executeCombinational()
    println(s"c --  ${program.dataInColumns}")
    program.dataStore.advanceBuffers()
    program.scheduler.executeBufferAdvances()
  }

  println(s"h --  ${program.header}")
  println(s"i --  ${program.dataInColumns}")

  poke("io_a", 33)
  poke("io_b", 11)
  poke("io_e", 1)

  println(s"p --  ${program.dataInColumns}")

  step()
  step()
  step()
  step()

  poke("io_e", 0)
  println(s"p --  ${program.dataInColumns}")

  var count = 0
  while(peek("io_v") == 0 && count < 50) {
    count += 1
    step()
  }

  println(timer.report())
}

object Compiler {
  def apply(input: String): Compiler = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val circuit = new Compiler(ast, Seq.empty)
    circuit
  }

  def main(args: Array[String]): Unit = {
    val fileName = args.headOption.getOrElse("GCD.fir")
    val text = io.Source.fromFile(fileName).getLines().mkString("\n")

    apply(text)
  }

}
