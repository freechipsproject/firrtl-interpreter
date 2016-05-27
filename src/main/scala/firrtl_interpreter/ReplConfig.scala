// See LICENSE for license details.

package firrtl_interpreter

case class ReplConfig(
                       allowCycles:      Boolean = false,
                       sortKeys:         Boolean = false,
                       firrtlSourceName: String  = "",
                       scriptName:       String  = ""
                 )
