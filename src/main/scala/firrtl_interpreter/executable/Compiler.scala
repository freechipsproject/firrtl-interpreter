// See LICENSE for license details.

//TODO:(chick) handle reset and multi-clock for registers
//TODO:(chick) shift amounts can only be int, this may reduce match statements

package firrtl_interpreter.executable

import firrtl._
import firrtl.ir.Circuit
import firrtl_interpreter.{DependencyTracker, FindModule, InterpreterException}

//scalastyle:off magic.number
class Compiler(ast: Circuit) {
  def lower(c: Circuit): Circuit = {
    val compiler = new LowFirrtlCompiler

    val annotationMap = AnnotationMap(Seq.empty)
    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, Some(annotationMap)))
    println(compileResult.emittedCircuitOption.get)
    compileResult.circuit
  }

  val loweredAst: Circuit = lower(ast)

  val compiler = new ExpressionCompiler(numberOfBuffers = 4)

  private val program = compiler.compile(loweredAst)

  private val dependencyTracker: DependencyTracker = {
    val module = FindModule(loweredAst.main, loweredAst) match {
      case regularModule: firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw InterpreterException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw InterpreterException(s"Top level module is not the right kind of module $x")
    }
    new DependencyTracker(loweredAst, module)
  }
  println(s"Dependency Tracker Info:\n${dependencyTracker.getInfo}")
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
  }

  println(s"h --  ${program.header}")
  println(s"i --  ${program.dataInColumns}")

  poke("io_a", 33)
  poke("io_b", 11)
  poke("io_e", 1)

  println(s"p --  ${program.dataInColumns}")

  step()
  step()

  poke("io_e", 0)
  println(s"p --  ${program.dataInColumns}")

  var count = 0
  while(peek("io_v") == 0 && count < 20) {
    count += 1
    step()
  }
}

object Compiler {
  def apply(input: String): Compiler = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val circuit = new Compiler(ast)
    circuit
  }

  def main(args: Array[String]): Unit = {
    val text = io.Source.fromFile("gcd.fir").getLines().mkString("\n")

    apply(text)
  }

}
