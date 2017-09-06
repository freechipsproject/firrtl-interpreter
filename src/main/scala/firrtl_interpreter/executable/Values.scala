// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter.InterpreterException

trait Value {
  def name: String
  def asBig: BigInt
  def isSigned: Boolean
  def intWidth: Int
  def isBig: Boolean = intWidth > Value.BigThreshold
}

class IntValue(val name: String, val isSigned: Boolean, val intWidth: Int) extends Value {
  var value: Int = 0
  override def asBig: BigInt = BigInt(value)
}

class BigValue(val name: String, val isSigned: Boolean, val intWidth: Int) extends Value {
  var value: Big = 0
  override def asBig: BigInt = value
}

object Value {
  val BigThreshold = 31

  def IntValue(name: String, firrtlType: firrtl.ir.Type): IntValue = {
    new IntValue(name, getIsSigned(name, firrtlType), getIntWidth(name, firrtlType))
  }

  def BigValue(name: String, firrtlType: firrtl.ir.Type): BigValue = {
    new BigValue(name, getIsSigned(name, firrtlType), getIntWidth(name, firrtlType))
  }

  def apply(name: String, firrtlType: firrtl.ir.Type): Value = {
    if(getIntWidth(name, firrtlType) > BigThreshold) {
      BigValue(name, firrtlType)
    }
    else {
      IntValue(name, firrtlType)
    }
  }

  def apply(name: String, isSigned: Boolean, width: Int): Value = {
    if(width > BigThreshold) {
      new BigValue(name, isSigned, width)
    }
    else {
      new IntValue(name, isSigned, width)
    }
  }

  def getIsSigned(name: String, tpe: firrtl.ir.Type): Boolean = {
    tpe match {
      case _: firrtl.ir.UIntType => false
      case _: firrtl.ir.SIntType => true
      case firrtl.ir.ClockType   => false
      case _ =>
        throw InterpreterException(s"Error: $name bad or unsupported type $tpe")
    }
  }
  def getIntWidth(name: String, tpe: firrtl.ir.Type): Int = {
    tpe match {
      case groundType: firrtl.ir.GroundType =>
        groundType.width match {
          case firrtl.ir.IntWidth(n) => n.toInt
          case _ =>
            throw InterpreterException(s"Error: $name bad or unsupported width $tpe")
        }
    }
  }
  def isBig(width: Int): Boolean = {
    width > BigThreshold
  }
}
