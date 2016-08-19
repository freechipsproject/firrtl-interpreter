// See LICENSE for license details.

package dsp.real

import firrtl_interpreter._

class DspRealAdd(val name: String) extends BlackBoxImplementation {
  def execute(inputValues: Seq[Concrete]): Concrete = {
    val arg1 :: arg2 :: tail = inputValues
    val doubleArg1 = java.lang.Double.longBitsToDouble(arg1.value.toLong)
    val doubleArg2 = java.lang.Double.longBitsToDouble(arg2.value.toLong)
    val doubleResult = doubleArg1 + doubleArg2
    val result = BigInt(java.lang.Double.doubleToLongBits(doubleResult))
    val long2 = arg2.value.toLong
    ConcreteUInt(result, 64)
  }
  def outputDependencies(outputName: String): Seq[(String)] = {
    outputName match {
      case "out" => Seq(fullName("in1"), fullName("in2"))
      case _ => Seq.empty
    }
  }
  def cycle(): Unit = {}
}

class DspRealFactory extends BlackBoxFactory {
  def appliesTo(blackBoxName: String): Boolean = {
    blackBoxName match {
      case "BBFAdd" => true
      case _        => false
    }
  }
  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    blackBoxName match {
      case "BBFAdd" => Some(add(new DspRealAdd(instanceName)))
      case _        => None
    }
  }
}