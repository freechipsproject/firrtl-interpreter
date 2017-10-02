// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ExecutionOptionsManager

case class ReplConfig(
    firrtlSourceName:  String  = "",
    scriptName:        String  = "",
    firrtlSource:      String = "",
    useVcdScript:      Boolean = false,
    vcdScriptOverride: String = "",
    runScriptAtStart:  Boolean = false)
  extends firrtl.ComposableOptions

trait HasReplConfig {
  self: ExecutionOptionsManager =>

  var replConfig = ReplConfig()

  parser.note("firrtl-repl")

  parser.opt[String]("fr-firrtl-source")
    .abbr("frfs")
    .valueName("<firrtl-source-file>")
    .foreach { x =>
      replConfig = replConfig.copy(firrtlSourceName = x)
    }
    .text("firrtl file to load on startup, default is no file")

  parser.opt[String]("fr-script-file")
    .abbr("frsf")
    .valueName("<firrtl-script-file>")
    .foreach { x =>
      replConfig = replConfig.copy(scriptName = x)
    }
    .text("script file to load on startup, default is no file")

  parser.opt[Unit]("fr-use-vcd-script")
    .abbr("fruvs")
    .foreach { _ =>
      replConfig = replConfig.copy(useVcdScript = true)
    }
    .text("Currently unsupported, use --fr-vcd-script-override <vcd-file-name> instead")

  parser.opt[String]("fr-vcd-script-override")
    .abbr("frvso")
    .valueName("<vcd-file>")
    .foreach { x =>
      replConfig = replConfig.copy(vcdScriptOverride = x, useVcdScript = true)
    }
    .text("load vcd file as script")

  parser.opt[Unit]("fr-run-script-on-startup")
    .abbr("frrsos")
    .foreach { _ =>
      replConfig = replConfig.copy(runScriptAtStart = true)
    }
    .text("run script immediately on startup")

  def getVcdFileName: String = {
    self.getBuildFileName("vcd", replConfig.vcdScriptOverride)
  }
}
