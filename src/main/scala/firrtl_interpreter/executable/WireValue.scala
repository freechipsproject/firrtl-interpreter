// See LICENSE for license details.

package firrtl_interpreter.executable

case class WireValue(name: String, isSigned: Boolean, bitSize: Int, index: Int) {
  def isBig: Boolean = bitSize > 32
}
