// See LICENSE for license details.

package firrtl_interpreter.real

import firrtl.ir.Type
import firrtl_interpreter._

abstract class DspRealTwoArgumentToDouble extends BlackBoxImplementation {
  /**
    * sub-classes must implement this two argument function
 *
    * @param double1 first operand
    * @param double2 second operand
    * @return        double operation result
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
    val arg1 :: arg2 :: _ = inputValues
    val doubleArg1 = bigIntBitsToDouble(arg1.value)
    val doubleArg2 = bigIntBitsToDouble(arg2.value)
    val doubleResult = twoOp(doubleArg1, doubleArg2)
    val result = doubleToBigIntBits(doubleResult)
    TypeInstanceFactory(tpe, result)
  }
}

abstract class DspRealTwoArgumentToBoolean extends BlackBoxImplementation {
  /**
    * sub-classes must implement this two argument function
    *
    * @param double1 first operand
    * @param double2 second operand
    * @return        boolean operation result
    */
  def twoOp(double1: Double, double2: Double): Boolean

  def outputDependencies(outputName: String): Seq[(String)] = {
    outputName match {
      case "out" => Seq(fullName("in1"), fullName("in2"))
      case _ => Seq.empty
    }
  }
  def cycle(): Unit = {}
  def execute(inputValues: Seq[Concrete], tpe: Type): Concrete = {
    val arg1 :: arg2 :: _ = inputValues
    val doubleArg1 = bigIntBitsToDouble(arg1.value)
    val doubleArg2 = bigIntBitsToDouble(arg2.value)
    val booleanResult = twoOp(doubleArg1, doubleArg2)
    val result = if(booleanResult) Big1 else Big0
    TypeInstanceFactory(tpe, result)
  }
}


class DspRealAdd(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 + double2
}

class DspRealSubtract(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 - double2
}

class DspRealMultiply(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 * double2
}

class DspRealDivide(val name: String) extends DspRealTwoArgumentToDouble {
  def twoOp(double1: Double, double2: Double): Double = double1 / double2
}

class DspRealGreaterThan(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 > double2
}

class DspRealGreaterThanEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 >= double2
}

class DspRealLessThan(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 < double2
}

class DspRealLessThanEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 <= double2
}

class DspRealEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 == double2
}

class DspRealNotEquals(val name: String) extends DspRealTwoArgumentToBoolean {
  def twoOp(double1: Double, double2: Double): Boolean = double1 != double2
}

//scalastyle:off cyclomatic.complexity
class DspRealFactory extends BlackBoxFactory {
  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation] = {
    blackBoxName match {
      case "BBFAdd"               => Some(add(new DspRealAdd(instanceName)))
      case "BBFSubtract"          => Some(add(new DspRealSubtract(instanceName)))
      case "BBFMultiply"          => Some(add(new DspRealMultiply(instanceName)))
      case "BBFDivide"            => Some(add(new DspRealDivide(instanceName)))
      case "BBFLessThan"          => Some(add(new DspRealLessThan(instanceName)))
      case "BBFLessThanEquals"    => Some(add(new DspRealLessThanEquals(instanceName)))
      case "BBFGreaterThan"       => Some(add(new DspRealGreaterThan(instanceName)))
      case "BBFGreaterThanEquals" => Some(add(new DspRealGreaterThanEquals(instanceName)))
      case "BBFEquals"            => Some(add(new DspRealEquals(instanceName)))
      case "BBFNotEquals"         => Some(add(new DspRealNotEquals(instanceName)))
      case _                      => None
    }
  }
}
//scalastyle:on cyclomatic.complexity
