// See LICENSE for license details.

//TODO:(chick) handle reset and multi-clock for registers

package firrtl_interpreter.executable

import firrtl._
import firrtl.ir.Circuit

//noinspection ScalaStyle
class Compiler(ast: Circuit) {
  def lower(c: Circuit): Circuit = {
      val compiler = new LowFirrtlCompiler

      val annotationMap = AnnotationMap(Seq.empty)
      val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, Some(annotationMap)))
      compileResult.circuit
    }

  val loweredAst: Circuit = lower(ast)

  val x = new ExpressionCompiler

  private val out = x.compile(loweredAst)

  def poke(name: String, value: Int): Unit = {
    out.namesToValues(name) match {
      case i: IntValue => i.value = value
      case i: BigValue => i.value = value
    }
  }

  def step(steps: Int = 1): Unit = {
    out.getTriggerExpressions.foreach { key => out.executeTriggeredAssigns(key) }
    out.executeCombinational()
  }

  println(out.header)
  println(out.toString)

  poke("io_a", 11)
  poke("io_b", 33)
  poke("io_e", 1)

  step()
  println(out.toString)

  poke("io_e", 0)
  step()
  println(out.toString)

  step()
  println(out.toString)

  step()
  println(out.toString)

  step()
  println(out.toString)

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
