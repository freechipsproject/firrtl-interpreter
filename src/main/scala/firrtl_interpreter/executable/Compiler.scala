// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl._
import firrtl.ir.Circuit
import firrtl_interpreter.{HasInterpreterOptions, ToLoFirrtl}

class Compiler(ast: Circuit) {
  def lower(c: Circuit): Circuit = {
      val compiler = new LowFirrtlCompiler

      val annotationMap = AnnotationMap(Seq.empty)
      val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, Some(annotationMap)))
      compileResult.circuit
    }


  val loweredAst: Circuit = lower(ast)

  val x = new ExpressionCompiler

  val out = x.compile(loweredAst)
}

object Compiler {

  def apply(input: String): Compiler = {
    val ast = firrtl.Parser.parse(input.split("\n").toIterator)
    val interpreter = new Compiler(ast)
    interpreter
  }

  def main(args: Array[String]): Unit = {
    val text = io.Source.fromFile("gcd.fir").getLines().mkString("\n")

    apply(text)
  }

}
