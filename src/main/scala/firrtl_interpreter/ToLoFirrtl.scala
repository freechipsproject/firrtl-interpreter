// See LICENSE for license details.

package firrtl_interpreter

import java.io.{StringWriter, Writer}

import firrtl.LowFirrtlCompiler
import firrtl.ir.Circuit

object ToLoFirrtl {
  def lower(c: Circuit): Circuit = {
    val compiler = new LowFirrtlCompiler

    val compileResult = compiler.compile(c, Seq(), new StringWriter())
    compileResult.circuit
  }
}
