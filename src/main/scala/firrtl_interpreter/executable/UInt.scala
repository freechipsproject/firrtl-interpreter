// See LICENSE for license details.

package firrtl_interpreter.executable

trait ExecutableValue {
  def name: String
  def asBigInt: BigInt
}

case class UInt(name: String, bitSize: Int) extends ExecutableValue {
  var value: Int = 0
  override def asBigInt: BigInt = BigInt(value)
}
