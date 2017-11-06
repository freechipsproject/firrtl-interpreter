// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.Kind
import firrtl.ir.{Info, IntWidth, NoInfo}
import firrtl_interpreter.InterpreterException

case class Symbol(
    name: String,
    dataSize:   DataSize,
    dataType:   DataType,
    dataKind:   Kind,
    bitWidth:   Int,
    slots:      Int,
    firrtlType: firrtl.ir.Type,
    info:       Info
) {
  var index:          Int = -1
  var cardinalNumber: Int = -1

  //  override def toString: String = {
  //    f"${s"$dataType<$bitWidth>"}%12s $dataSize index $index%5d $name%-40.40s"
  //  }
  def render: String = {
    f"$name%-40.40s $dataSize%3.3s $dataType%4.4s $bitWidth%6d $slots%6d $index%6d $cardinalNumber%6d $info"
  }
}

object Symbol {
  def apply(name: String, firrtlType: firrtl.ir.Type, firrtlKind: Kind, slots: Int = 1, info: Info = NoInfo): Symbol = {
    Symbol(name, DataSize(firrtlType), DataType(firrtlType),
      firrtlKind, DataSize.getBitWidth(firrtlType), slots, firrtlType, info)
  }

  def renderHeader: String = {
    f"""${"name"}%-40.40s ${"Bin"}%3.3s ${"Type"}%4.4s ${"Width"}%6s ${"Slots"}%6s ${"Index"}%6s ${"Depend"}%6s Info"""
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
  val LongThreshold = 63

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
