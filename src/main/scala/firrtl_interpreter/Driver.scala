/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package firrtl_interpreter

import firrtl.{ExecutionOptionsManager, HasFirrtlOptions}

case class InterpreterOptions(
    writeVCD:          Boolean              = false,
    vcdShowUnderscored:Boolean              = false,
    setVerbose:        Boolean              = false,
    setOrderedExec:    Boolean              = false,
    allowCycles:       Boolean              = false,
    randomSeed:        Long                 = System.currentTimeMillis(),
    blackBoxFactories: Seq[BlackBoxFactory] = Seq.empty,
    maxExecutionDepth: Long                 = ExpressionExecutionStack.defaultMaxExecutionDepth,
    showFirrtlAtLoad:  Boolean              = false,
    lowCompileAtLoad:  Boolean              = true,
    // monitor options
    monitorReportFileName:  String          = "",
    monitorBitUsage:        Boolean         = false,
    monitorHistogramBins:  Int             = 0,
    monitorTrackTempNodes:  Boolean         = false,
    prettyPrintReport:      Boolean         = false
    )
  extends firrtl.ComposableOptions {

  def vcdOutputFileName(optionsManager: ExecutionOptionsManager): String = {
    if(writeVCD) {
      s"${optionsManager.getBuildFileName("vcd")}"
    }
    else {
      ""
    }
  }

  def getMonitorReportFile(optionsManager: ExecutionOptionsManager with HasInterpreterOptions): String = {
    if(optionsManager.interpreterOptions.monitorReportFileName.nonEmpty) {
      optionsManager.getBuildFileName(optionsManager.interpreterOptions.monitorReportFileName)
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
    .text("writes vcd execution log, filename will be based on top circuit name")

  parser.opt[Unit]("fint-vcd-show-underscored-vars")
    .abbr("fivsuv")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(vcdShowUnderscored = true)
    }
    .text("vcd output by default does not show var that start with underscore, this overrides that")

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

  parser.opt[Unit]("fr-allow-cycles")
    .abbr("fiac")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(allowCycles = true)
    }
    .text(s"allow combinational loops to be processed, though unreliable, default is ${interpreterOptions.allowCycles}")

  parser.opt[Long]("fint-random-seed")
    .abbr("firs")
      .valueName("<long-value>")
    .foreach { x =>
      interpreterOptions = interpreterOptions.copy(randomSeed = x)
    }
    .text("seed used for random numbers generated for tests and poison values, default is current time in ms")

  parser.opt[Long]("fint-max-execution-depth")
    .abbr("fimed")
    .valueName("<long-value>")
    .foreach { x =>
      interpreterOptions = interpreterOptions.copy(maxExecutionDepth = x)
    }
    .text("depth of stack used to evaluate expressions")

  parser.opt[Unit]("show-firrtl-at-load")
    .abbr("fisfas")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(showFirrtlAtLoad = true)
    }
    .text("compiled low firrtl at firrtl load time")

  parser.opt[Unit]("dont-run-lower-compiler-on-load")
    .abbr("filcol")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(lowCompileAtLoad = false)
    }
    .text("run lowering compuler when firrtl file is loaded")

  parser.note("firrtl-interpreter-options-bit-usage-monitoring")

  parser.opt[Unit]("fint-monitor-bit-usage")
    .abbr("fimbu")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(monitorBitUsage = true)
    }
    .text("turn on bit monitoring, turns on stats for wires")

  parser.opt[String]("fint-monitor-output-file")
    .abbr("fimof")
    .valueName("<name-of-output-file>")
    .foreach { x =>
      interpreterOptions = interpreterOptions.copy(monitorReportFileName = x)
    }
    .text("write monitoring stats to this file name instead of standard out")

  parser.opt[Int]("fint-monitor-histogram-bins")
    .abbr("fimhb")
    .valueName("<number-of-bins>")
    .foreach { x =>
      interpreterOptions = interpreterOptions.copy(monitorHistogramBins = x)
    }
    .text("keeps this number of bins for histogram of bit usages, must be power of 2, default is none")

  parser.opt[Unit]("fint-monitor-track-temps")
    .abbr("fitt")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(monitorTrackTempNodes = true)
    }
    .text("keep bit usage stats for temp nodes, default is false")

  parser.opt[Unit]("fint-pretty-print-monitor-stats")
    .abbr("fippms")
    .foreach { _ =>
      interpreterOptions = interpreterOptions.copy(prettyPrintReport = true)
    }
    .text("columnizes bit usage monitor reports")

}

object Driver {

  def execute(firrtlInput: String, optionsManager: InterpreterOptionsManager): Option[InterpretiveTester] = {
    val tester = new InterpretiveTester(firrtlInput, optionsManager)
    Some(tester)
  }

  def execute(args: Array[String], firrtlInput: String): Option[InterpretiveTester] = {
    val optionsManager = new InterpreterOptionsManager

    if (optionsManager.parser.parse(args)) {
      execute(firrtlInput, optionsManager)
    } else {
      None
    }
  }
}

class InterpreterOptionsManager extends ExecutionOptionsManager("interpreter") with HasInterpreterSuite

trait HasInterpreterSuite extends ExecutionOptionsManager with HasFirrtlOptions with HasInterpreterOptions {
  self : ExecutionOptionsManager =>
}
