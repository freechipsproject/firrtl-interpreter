// See LICENSE for license details.

package firrtl_interpreter.real

import firrtl.ir.Type
import firrtl_interpreter._

abstract class DspRealTwoArgumentImplementation extends BlackBoxImplementation {
  /**
    * sub-classes must implement this two argument function
    * @param double1
    * @param double2
    * @return
    */
  def twoOp(double1: Double, double2: Double): Double

  def outputDependencies(outputName: String): Seq[(String)] = {
    outputName match {
      case "out" => Seq(fullName("in1"), fullName("in2"))
      case _ => Seq.empty
    }
  }
  def cycle(): Unit = {}
  def execute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    val arg1 :: arg2 :: tail = inputValues
    val doubleArg1 = bigIntBitsToDouble(arg1.value)
    val doubleArg2 = bigIntBitsToDouble(arg2.value)
    val doubleResult = twoOp(doubleArg1, doubleArg2)
    val result = doubleToBigIntBits(doubleResult)
    TypeInstanceFactory(tpe, result)
  }
}

class DspRealAdd(val name: String) extends DspRealTwoArgumentImplementation {
  def twoOp(double1: Double, double2: Double): Double = double1 + double2
}

class DspRealMultiply(val name: String) extends DspRealTwoArgumentImplementation {
  def twoOp(double1: Double, double2: Double): Double = double1 * double2
}

class DspRealFactory extends BlackBoxFactory {
  def appliesTo(blackBoxName: String): Boolean = {
    blackBoxName match {
      case "BBFAdd"      => true
      case "BBFMultiply" => true
      case _             => false
    }
  }
  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    blackBoxName match {
      case "BBFAdd"      => Some(add(new DspRealAdd(instanceName)))
      case "BBFMultiply" => Some(add(new DspRealMultiply(instanceName)))
      case _             => None
    }
  }
}