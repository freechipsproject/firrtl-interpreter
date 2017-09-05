// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

class ExecutableCircuit {
  val namesToValues:    mutable.HashMap[String, Value]    = new mutable.HashMap[String, Value]

  val combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]

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
      case (v: BigValue, e: IntExpressionResult) => AssignBig(v, ToBig(e.apply).apply)
      case (v: BigValue, e: BigExpressionResult) => AssignBig(v, e.apply)
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

  //  def header: String = {
//    names.keys.toArray.sorted.map { name => f"$name%10.10s" }.mkString("")
//  }
//
//  def addWire(wireValue: ExecutableValue): ExecutableValue = {
//    names(wireValue.name) = wireValue
//    wireValue
//  }
//
//  def apply(name: String): ExecutableValue = {
//    names(name)
//  }
//
//  def getUInt(name: String): IntValue = {
//    names(name).asInstanceOf[IntValue]
//  }
//
//  override def toString: String = {
//    names.keys.toArray.sorted.map { key => f"${names(key).asBigInt}%10d" }.mkString("")
//  }
}
