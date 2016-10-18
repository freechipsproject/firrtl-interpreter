// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ExecutionOptionsManager

case class InterpreterOptions(
    writeVCD: Boolean = false,
    setVerbose: Boolean = false,
    setOrderedExec: Boolean = false)
  extends firrtl.ComposableOptions {

  def vcdOutputFileName(optionsManager: ExecutionOptionsManager): String = {
    if(writeVCD) {
      s"${optionsManager.getBuildFileName("vcd")}"
    }
    else {
      ""
    }
  }
}

trait HasInterpreterOptions {
  self: ExecutionOptionsManager =>

  var interpreterOptions = InterpreterOptions()

  parser.note("firrtl-interpreter-options")

  parser.opt[Unit]("fint-write-vcd")
    .abbr("fiwv")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(writeVCD = true)
    }
    .text("writes vcd execution log, filename will be base on top")

  parser.opt[Unit]("fint-verbose")
    .abbr("fiv")
    .foreach {_ =>
      interpreterOptions = interpreterOptions.copy(setVerbose = true)
    }
    .text("makes interpreter very verbose")

  parser.opt[Unit]("fint-ordered-exec")
    .abbr("fioe")
    .foreach { _ =>
    interpreterOptions = interpreterOptions.copy(setOrderedExec = true)
  }
    .text("operates on dependencies optimally, can increase overhead, makes verbose mode easier to read")


}

object Driver {

  def execute(
      firrtlInput: String,
      optionsManager: ExecutionOptionsManager with HasInterpreterOptions): Option[InterpretiveTester] = {
    val tester = new InterpretiveTester(firrtlInput, optionsManager)

    Some(tester)
  }

  def execute(
               args: Array[String],
               firrtlInput: String
             ): Option[InterpretiveTester] = {
    val optionsManager = new ExecutionOptionsManager("firrtl-interpreter") with HasInterpreterOptions

    optionsManager.parser.parse(args) match {
      case true =>
        execute(firrtlInput, optionsManager)
      case _ =>
        None
    }
  }
}