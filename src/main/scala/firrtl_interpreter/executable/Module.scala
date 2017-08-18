// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

case class Module(parentOpt: Option[Module]) {
  lazy val (ints: Array[Int], bigs: Array[BigInt]) = createValues

  val intBuilder = new mutable.ArrayBuffer[Int]
  val bigBuilder = new mutable.ArrayBuffer[BigInt]
  def createValues: (Array[Int], Array[BigInt]) = {
    (Array.fill(10) { 0 }, Array.fill(20) { BigInt(0) })
  }

}
