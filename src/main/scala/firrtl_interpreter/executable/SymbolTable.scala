// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir.IntWidth
import firrtl_interpreter.InterpreterException

import scala.collection.mutable

class SymbolTable {
  private val table = new mutable.HashMap[String, Symbol]
  private val nextIndexFor = new mutable.HashMap[DataSize, Int]
  nextIndexFor(IntSize) = 0
  nextIndexFor(LongSize) = 0
  nextIndexFor(BigSize) = 0

  def size: Int = table.size

  def getSizes: (Int, Int, Int) = {
    (nextIndexFor(IntSize), nextIndexFor(LongSize), nextIndexFor(BigSize))
  }

  /**
    * Based on width allocate slots at the current index, bumping the index for
    * the next call.
    * @param dataSize Used to determine which index to use.
    * @param slots not sure if we need this but allows multiple assigns with 1 call
    * @return the index assigned
    */
  def getIndex(dataSize: DataSize, slots: Int = 1): Int = {
    val index = nextIndexFor(dataSize)
    nextIndexFor(dataSize) += slots
    index
  }

  def assignIndex(symbol: Symbol): Unit = {
    symbol.index = getIndex(symbol.dataSize)
  }

  def addSymbol(symbol: Symbol): Symbol = {
    table(symbol.name) = symbol
    assignIndex(symbol)
    symbol
  }

  def addSymbol(name: String, dataSize: DataSize, dataType: DataType, bitWidth: Int): Symbol = {
    addSymbol(Symbol(name, dataSize, dataType, bitWidth))
  }
}

case class Symbol(name: String, dataSize: DataSize, dataType: DataType, bitWidth: Int) {
  var index: Int = -1
}

object Symbol {
  def apply(name: String, firrtlType: firrtl.ir.Type): Symbol = {
    Symbol(name, DataSize(firrtlType), DataType(firrtlType), DataSize.getBitWidth(firrtlType))
  }
}

trait DataSize

object IntSize extends DataSize {
  override def toString: String = "Int"
}
object LongSize extends DataSize {
  override def toString: String = "Long"
}
object BigSize extends DataSize {
  override def toString: String = "Big"
}

object DataSize {
  val IntThreshold = 31
  val LongThreshold = 31 // Not being used right now

  def getBitWidth(firrtlType: firrtl.ir.Type): Int = {
    firrtlType match {
      case firrtl.ir.SIntType(IntWidth(bitWidth)) => bitWidth.toInt
      case firrtl.ir.UIntType(IntWidth(bitWidth)) => bitWidth.toInt
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

  def apply(firrtlType: firrtl.ir.Type): DataSize = {
    apply(getBitWidth(firrtlType))
  }
}

trait DataType

object SignedInt extends DataType {
  override def toString: String = "SInt"
}
object UnsignedInt extends DataType {
  override def toString: String = "UInt"
}

object DataType {
  //TODO: (chick) do we need clock and reset types here

  def apply(tpe: firrtl.ir.Type): DataType = {
    tpe match {
      case _: firrtl.ir.SIntType => SignedInt
      case _: firrtl.ir.UIntType => UnsignedInt
      case t =>
        throw new InterpreterException(s"DataType does not know firrtl type $t")
    }
  }
}
