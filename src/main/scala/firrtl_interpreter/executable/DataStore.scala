// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter.InterpreterException

import scala.collection.mutable

/**
  * Creates a data store for the three underlying data types.
  * The numberOfBuffers is used to control the ability to rollback execution.
  * The meaning of the values of each slot must be maintained outside of this class.
  * This class only supports (2 ** 31) - 1 of any ints, longs or bigs.
  *
  * @param numberOfBuffers Number of buffers
  */
class DataStore(val numberOfBuffers: Int) {
  assert(numberOfBuffers > 0, s"DataStore: numberOfBuffers $numberOfBuffers must be > 0")

  private val nextIndexFor = new mutable.HashMap[DataSize, Int]
  nextIndexFor(IntSize)  = 0
  nextIndexFor(LongSize) = 0
  nextIndexFor(BigSize)  = 0

  def numberOfInts: Int  = nextIndexFor(IntSize)
  def numberOfLongs: Int = nextIndexFor(LongSize)
  def numberOfBigs: Int  = nextIndexFor(BigSize)

  def getSizes: (Int, Int, Int) = {
    (nextIndexFor(IntSize), nextIndexFor(LongSize), nextIndexFor(BigSize))
  }

  def getIndex(dataSize: DataSize, slots: Int = 1): Int = {
    val index = nextIndexFor(dataSize)
    nextIndexFor(dataSize) += slots
    index
  }

  var intData:  Array[Array[Int]]  = Array.fill(numberOfBuffers, numberOfInts)(0)
  var longData: Array[Array[Long]] = Array.fill(numberOfBuffers, numberOfLongs)(0L)
  var bigData:  Array[Array[Big]]  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

  var currentBufferIndex:  Int = if(numberOfBuffers > 1) 1 else 0
  var previousBufferIndex: Int = 0

  var currentIntArray:   Array[Int]  = intData(currentBufferIndex)
  var currentLongArray:  Array[Long] = longData(currentBufferIndex)
  var currentBigArray:   Array[Big]  = bigData(currentBufferIndex)
  var previousIntArray:  Array[Int]  = intData(previousBufferIndex)
  var previousLongArray: Array[Long] = longData(previousBufferIndex)
  var currentBigSource:  Array[Big]  = bigData(previousBufferIndex)

  def allocateBuffers(): Unit = {
    intData  = Array.fill(numberOfBuffers, numberOfInts)(0)
    longData = Array.fill(numberOfBuffers, numberOfLongs)(0L)
    bigData  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

    currentIntArray  = intData(currentBufferIndex)
    currentLongArray = longData(currentBufferIndex)
    currentBigArray  = bigData(currentBufferIndex)
    previousIntArray  = intData(previousBufferIndex)
    previousLongArray = longData(previousBufferIndex)
    currentBigSource  = bigData(previousBufferIndex)
  }

  /**
    * Get the three source buffers
    * @return
    */
  def sourceBuffers(): (Array[Int], Array[Long], Array[Big]) = {
    (intData(previousBufferIndex), longData(previousBufferIndex), bigData(previousBufferIndex))
  }

  /**
    * Get the three target buffers
    * @return
    */
  def targetBuffers(): (Array[Int], Array[Long], Array[Big]) = {
    (intData(currentBufferIndex), longData(currentBufferIndex), bigData(currentBufferIndex))
  }

  /**
    * Advance the buffers if you are using more than 1.
    * TODO: When the heck do you call this thing
    */
  def advanceBuffers(): Unit = {
    if(numberOfBuffers > 1) {
      previousBufferIndex = (previousBufferIndex + 1) % numberOfBuffers
      currentBufferIndex = (currentBufferIndex + 1) % numberOfBuffers

      currentIntArray  = intData(currentBufferIndex)
      currentLongArray = longData(currentBufferIndex)
      currentBigArray  = bigData(currentBufferIndex)
      previousIntArray  = intData(previousBufferIndex)
      previousLongArray = longData(previousBufferIndex)
      currentBigSource  = bigData(previousBufferIndex)

      for(i <- currentIntArray.indices) { currentIntArray(i) = previousIntArray(i) }
    }
  }

  case class GetInt(index: Int) extends IntExpressionResult {
    def apply(): Int = currentIntArray(index)
  }
  case class AssignInt(index: Int, expression: FuncInt) extends Assigner {
    val m = Map(22 -> "x/in", 14 -> "x", 11 -> "y", 8 -> "y/in" )
    def apply(): Unit = {
//      if(m.contains(index)) {
        println(s"${m.getOrElse(index, index)} <= ${expression()}")
//      }
      currentIntArray(index) = expression()
    }
  }

  case class GetLong(index: Int) extends LongExpressionResult {
    def apply(): Long = currentLongArray(index)
  }
  case class AssignLong(index: Int, expression: FuncLong) extends Assigner {
    def apply(): Unit = {
      currentLongArray(index) = expression()
    }
  }

  case class GetBig(index: Int) extends BigExpressionResult {
    def apply(): Big = currentBigArray(index)
  }
  case class AssignBig(index: Int, expression: FuncBig) extends Assigner {
    def apply(): Unit = {
      currentBigArray(index) = expression()
    }
  }

  def getSizeAndIndex(assigner: Assigner): (DataSize, Int) = {
    assigner match {
      case assign: AssignInt => (IntSize, assign.index)
      case assign: AssignLong => (LongSize, assign.index)
      case assign: AssignBig => (BigSize, assign.index)
      case assign =>
        throw InterpreterException(s"unknown assigner found $assign")
    }
  }

  def getIntRow(index: Int): Seq[Int] = {
    intData.indices.map { historyIndex =>
      val adjustedHistoryIndex = (historyIndex + currentBufferIndex) % numberOfBuffers
      intData(adjustedHistoryIndex)(index)
    }
  }

  def apply(symbol: Symbol): Big = {
    symbol.dataSize match {
      case IntSize  => currentIntArray(symbol.index)
      case LongSize => currentLongArray(symbol.index)
      case BigSize  => currentBigArray(symbol.index)
    }
  }

  def update(symbol: Symbol, value: Big): Unit = {
    symbol.dataSize match {
      case IntSize  => currentIntArray(symbol.index) = value.toInt
      case LongSize => currentLongArray(symbol.index) = value.toLong
      case BigSize  => currentBigArray(symbol.index) = value
    }
  }
}

object DataStore {
  def apply(numberOfBuffers: Int): DataStore = {
    new DataStore(numberOfBuffers)
  }
}
