// See LICENSE for license details.

package firrtl_interpreter

import firrtl._
import firrtl.ir.Circuit

object ToLoFirrtl {
  def lower(c: Circuit, annotations: AnnotationSeq): Circuit = {
    val compiler = new LowFirrtlCompiler

    val compileResult = compiler.compileAndEmit(firrtl.CircuitState(c, ChirrtlForm, annotations))
    compileResult.circuit
  }
}
