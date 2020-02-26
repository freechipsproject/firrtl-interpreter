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
