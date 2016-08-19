// See LICENSE for license details.

package firrtl_interpreter

import scala.collection.mutable.ArrayBuffer

case class ReplConfig(
                       allowCycles:       Boolean = false,
                       sortKeys:          Boolean = false,
                       firrtlSourceName:  String  = "",
                       scriptName:        String  = "",
                       dspSupport:        Boolean = false
                     )
