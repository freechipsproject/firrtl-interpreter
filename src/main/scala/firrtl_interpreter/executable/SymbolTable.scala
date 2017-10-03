// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir.IntWidth
import firrtl_interpreter.InterpreterException

import scala.collection.mutable

class SymbolTable(dataStore: DataStore) {
  private val table = new mutable.HashMap[String, Symbol]
  private val sizeAndIndexToSymbol = new mutable.HashMap[DataSize, mutable.HashMap[Int, Symbol]] {
    override def default(key: DataSize): mutable.HashMap[Int, Symbol] = {
      this(key) = new mutable.HashMap[Int, Symbol]
      this(key)
    }
  }

  def size: Int = table.size
  def keys:Iterable[String] = table.keys

  def getSizes: (Int, Int, Int) = {
    dataStore.getSizes
  }

  /**
    * Based on width allocate slots at the current index, bumping the index for
    * the next call.
    * @param dataSize Used to determine which index to use.
    * @param slots not sure if we need this but allows multiple assigns with 1 call
    * @return the index assigned
    */
  def getIndex(dataSize: DataSize, slots: Int = 1): Int = dataStore.getIndex(dataSize, slots)

  def assignIndex(symbol: Symbol): Unit = {
    symbol.index = getIndex(symbol.dataSize)
    sizeAndIndexToSymbol(symbol.dataSize)(symbol.index) = symbol
  }

  def addSymbol(symbol: Symbol): Symbol = {
    if(!table.contains(symbol.name)) {
      table(symbol.name) = symbol
      assignIndex(symbol)
    }
    symbol
  }

  def addSymbol(name: String, firrtlType: firrtl.ir.Type): Symbol = {
    if(!table.contains(name)) {
      addSymbol(Symbol(name, firrtlType))
    }
    else {
      table(name)
    }
  }

  def getSymbol(name: String, firrtlType: firrtl.ir.Type): Symbol = {
    if(table.contains(name)) {
      table(name)
    }
    else {
      addSymbol(Symbol(name, firrtlType))
    }
  }

  def getSymbol(name: String, dataSize: DataSize, dataType: DataType, bitWidth: Int): Symbol = {
    if(table.contains(name)) {
      table(name)
    }
    else {
      addSymbol(Symbol(name, dataSize, dataType, bitWidth))
    }
  }

  val registerNames: mutable.HashSet[String] = new mutable.HashSet[String]

  def isRegister(name: String): Boolean = registerNames.contains(name)

  def apply(name: String): Symbol = table(name)
  def apply(dataSize: DataSize, index: Int): Symbol = sizeAndIndexToSymbol(dataSize)(index)

  def render: String = {
    keys.toArray.sorted.map { name =>
      table(name)
    }.mkString("\n")
  }
}

object SymbolTable {
  def apply(dataStore: DataStore): SymbolTable = new SymbolTable(dataStore)
}

case class Symbol(name: String, dataSize: DataSize, dataType: DataType, bitWidth: Int) extends Ordering[Symbol] {
  var index: Int = -1

  override def hashCode(): Int = {
    name.hashCode
  }
  override def equals(that: Any): Boolean = that match {
    case that: Symbol => this.hashCode() == that.hashCode()
    case _ =>
      throw new InterpreterException((s"Can't compare Symbol $this to $that"))
  }

//  override def toString: String = {
//    f"${s"$dataType<$bitWidth>"}%12s $dataSize index $index%5d $name%-40.40s"
//  }

  override def compare(x: Symbol, y: Symbol): Int = {
    if(x == y) { 0 }
    else if(x < y) { -1 }
    else { 1 }
  }
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
      case firrtl.ir.ClockType   => UnsignedInt
      case t =>
        throw new InterpreterException(s"DataType does not know firrtl type $t")
    }
  }
}
