// See LICENSE for license details.

package firrtl_interpreter

import java.io.{StringWriter, Writer}

import firrtl.{LowFirrtlCompiler, Circuit}
import firrtl.passes._

/**
  * Created by chick on 4/21/16.
  */
object ToLoFirrtl {
  def lower(c: Circuit): Circuit = {
    val compiler = new LowFirrtlCompiler

    val compileResult = compiler.compile(c, Seq(), new StringWriter())
    compileResult.circuit
  }
}
