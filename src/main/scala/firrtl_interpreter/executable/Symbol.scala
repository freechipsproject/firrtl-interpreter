// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir.IntWidth
import firrtl_interpreter.InterpreterException

case class Symbol(name: String, dataSize: DataSize, dataType: DataType, bitWidth: Int, slots: Int = 1) {
  var index: Int = -1
  var cardinalNumber: Int = -1

  //  override def toString: String = {
  //    f"${s"$dataType<$bitWidth>"}%12s $dataSize index $index%5d $name%-40.40s"
  //  }
}

object Symbol {
  def apply(name: String, firrtlType: firrtl.ir.Type): Symbol = {
    Symbol(name, DataSize(firrtlType), DataType(firrtlType), DataSize.getBitWidth(firrtlType))
  }
}

trait DataSize

case object IntSize extends DataSize {
  override def toString: String = "Int"
}
case object LongSize extends DataSize {
  override def toString: String = "Long"
}
case object BigSize extends DataSize {
  override def toString: String = "Big"
}

object DataSize {
  val IntThreshold = 31
  val LongThreshold = 31 // Not being used right now

  def getBitWidth(firrtlType: firrtl.ir.Type): Int = {
    firrtlType match {
      case firrtl.ir.SIntType(IntWidth(bitWidth)) => bitWidth.toInt
      case firrtl.ir.UIntType(IntWidth(bitWidth)) => bitWidth.toInt
      case firrtl.ir.ClockType                    => 1
      case _ =>
        throw InterpreterException(s"Error:DataSize doesn't know size of $firrtlType")
    }
  }

  def apply(bitWidth: Int): DataSize = {
    if(bitWidth <= DataSize.IntThreshold) {
      IntSize
    }
    else if(bitWidth <= DataSize.LongThreshold) {
      LongSize
    }
    else {
      BigSize
    }
  }

  def apply(bitWidth: BigInt): DataSize = apply(bitWidth.toInt)

  def apply(firrtlType: firrtl.ir.Type): DataSize = {
    apply(getBitWidth(firrtlType))
  }
}

trait DataType

case object SignedInt extends DataType {
  override def toString: String = "SInt"
}
case object UnsignedInt extends DataType {
  override def toString: String = "UInt"
}

object DataType {
  //TODO: (chick) do we need clock and reset types here

  def apply(tpe: firrtl.ir.Type): DataType = {
    tpe match {
      case _: firrtl.ir.SIntType => SignedInt
      case _: firrtl.ir.UIntType => UnsignedInt
      case firrtl.ir.ClockType   => UnsignedInt
      case t =>
        throw new InterpreterException(s"DataType does not know firrtl type $t")
    }
  }
}
