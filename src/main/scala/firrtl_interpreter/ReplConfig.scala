// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ExecutionOptionsManager

case class ReplConfig(
    firrtlSourceName:  String  = "",
    scriptName:        String  = "",
    firrtlSource:      String = "")
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

}
