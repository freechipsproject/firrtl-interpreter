// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter.InterpreterException

import scala.collection.mutable

class ExecutableCircuit {
  val namesToValues:    mutable.HashMap[String, Value]    = new mutable.HashMap[String, Value]

  val combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]

  val registerNames: mutable.HashSet[String] = new mutable.HashSet[String]

  val clockAssigns: mutable.HashMap[ExpressionResult, mutable.ArrayBuffer[Assigner]] = {
    new mutable.HashMap[ExpressionResult, mutable.ArrayBuffer[Assigner]] {
      override def default(key: ExpressionResult): mutable.ArrayBuffer[Assigner] = {
        this(key) = new mutable.ArrayBuffer[Assigner]()
        this(key)
      }
    }
  }

  def newValue(name: String, tpe: firrtl.ir.Type): Value = {
    namesToValues.get(name) match {
      case Some(intValue) => intValue
      case _ =>
        val value = Value(name, tpe)
        namesToValues(name) = value
        value
    }
  }

  def newValue(name: String, isSigned: Boolean, width: Int): Value = {
    namesToValues.get(name) match {
      case Some(intValue) => intValue
      case _ =>
        val value = Value(name, isSigned, width)
        namesToValues(name) = value
        value
    }
  }

  def assign(value: Value, expressionResult: ExpressionResult): Unit = {
    val assignment = (value, expressionResult) match {
      case (v: IntValue, e: IntExpressionResult) => AssignInt(v, e.apply)
      case (v: IntValue, e: BigExpressionResult) => AssignInt(v, ToInt(e.apply).apply)
      case (v: BigValue, e: IntExpressionResult) => AssignBig(v, ToBig(e.apply).apply)
      case (v: BigValue, e: BigExpressionResult) => AssignBig(v, e.apply)
      case _ =>
        throw InterpreterException(s"what's going on $value $expressionResult")
    }
    combinationalAssigns += assignment
  }

  def clockAssign(clockExpression: ExpressionResult, value: Value, expressionResult: ExpressionResult): Unit = {
    val assignment = (value, expressionResult) match {
      case (v: IntValue, e: IntExpressionResult) => AssignInt(v, e.apply)
      case (v: BigValue, e: IntExpressionResult) => AssignBig(v, ToBig(e.apply).apply)
      case (v: BigValue, e: BigExpressionResult) => AssignBig(v, e.apply)
    }
    clockAssigns(clockExpression) += assignment
  }

  def header: String = {
    namesToValues.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
  }

  def addWire(wireValue: Value): Value = {
    namesToValues(wireValue.name) = wireValue
    wireValue
  }

  def apply(name: String): Value = {
    namesToValues(name)
  }

  def getUInt(name: String): IntValue = {
    namesToValues(name).asInstanceOf[IntValue]
  }

  override def toString: String = {
    namesToValues.keys.toArray.sorted.map { key => f"${namesToValues(key).asBig}%10d" }.mkString("")
  }
}
