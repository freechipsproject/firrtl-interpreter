// See LICENSE for license details.

package firrtl_interpreter

import java.io.{StringWriter, Writer}

import firrtl.LowFirrtlCompiler
import firrtl.ir.Circuit
import firrtl.Annotations.AnnotationMap

object ToLoFirrtl {
  def lower(c: Circuit): Circuit = {
    val compiler = new LowFirrtlCompiler

    val compileResult = compiler.compile(c, new AnnotationMap(Seq.empty), new StringWriter())
    compileResult.circuit
  }
}
