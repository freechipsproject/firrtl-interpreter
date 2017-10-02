// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

/**
  * Creates a data store for the three underlying data types.
  * The numberOfBuffers is used to control the ability to rollback execution.
  * The meaning of the values of each slot must be maintained outside of this class.
  * This class only supports (2 ** 31) - 1 of any ints, longs or bigs.
  *
  * @param numberOfBuffers Number of buffers
  */
class DataStore(numberOfBuffers: Int) {
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

  private var targetBufferIndex = if(numberOfBuffers > 1) 1 else 0
  private var sourceBufferIndex = 0

  private var currentIntTarget  = intData(targetBufferIndex)
  private var currentLongTarget = longData(targetBufferIndex)
  private var currentBigTarget  = bigData(targetBufferIndex)
  private var currentIntSource  = intData(sourceBufferIndex)
  private var currentLongSource = longData(sourceBufferIndex)
  private var currentBigSource  = bigData(sourceBufferIndex)

  def allocateBuffers(): Unit = {
    intData  = Array.fill(numberOfBuffers, numberOfInts)(0)
    longData = Array.fill(numberOfBuffers, numberOfLongs)(0L)
    bigData  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

    currentIntTarget  = intData(targetBufferIndex)
    currentLongTarget = longData(targetBufferIndex)
    currentBigTarget  = bigData(targetBufferIndex)
    currentIntSource  = intData(sourceBufferIndex)
    currentLongSource = longData(sourceBufferIndex)
    currentBigSource  = bigData(sourceBufferIndex)
  }

  /**
    * Get the three source buffers
    * @return
    */
  def sourceBuffers(): (Array[Int], Array[Long], Array[Big]) = {
    (intData(sourceBufferIndex), longData(sourceBufferIndex), bigData(sourceBufferIndex))
  }

  /**
    * Get the three target buffers
    * @return
    */
  def targetBuffers(): (Array[Int], Array[Long], Array[Big]) = {
    (intData(targetBufferIndex), longData(targetBufferIndex), bigData(targetBufferIndex))
  }

  /**
    * Advance the buffers if you are using more than 1.
    * TODO: When the heck do you call this thing
    */
  def advanceBuffers(): Unit = {
    if(numberOfBuffers > 1) {
      sourceBufferIndex = (sourceBufferIndex + 1) % numberOfBuffers
      targetBufferIndex = (targetBufferIndex + 1) % numberOfBuffers

      currentIntTarget  = intData(targetBufferIndex)
      currentLongTarget = longData(targetBufferIndex)
      currentBigTarget  = bigData(targetBufferIndex)
      currentIntSource  = intData(sourceBufferIndex)
      currentLongSource = longData(sourceBufferIndex)
      currentBigSource  = bigData(sourceBufferIndex)
    }
  }

  case class GetInt(index: Int) extends IntExpressionResult {
    def apply(): Int = currentIntSource(index)
  }
  case class AssignInt(index: Int, expression: FuncInt) extends Assigner {
    def apply(): Unit = {
      currentIntTarget(index) = expression()
    }
  }

//  case class GetLong(index: Int) extends LongExpressionResult {
//    def apply(): Long = currentLongSource(index)
//  }
//  case class AssignLong(index: Int, expression: FuncLong) extends Assigner {
//    def apply(): Unit = {
//      currentLongTarget(index) = expression()
//    }
//  }

  case class GetBig(index: Int) extends BigExpressionResult {
    def apply(): Big = currentBigSource(index)
  }
  case class AssignBig(index: Int, expression: FuncBig) extends Assigner {
    def apply(): Unit = {
      currentBigTarget(index) = expression()
    }
  }

  def getIntRow(index: Int): Seq[Int] = {
    intData.indices.map { historyIndex =>
      val adjustedHistoryIndex = (historyIndex + targetBufferIndex) % numberOfBuffers
      intData(adjustedHistoryIndex)(index)
    }
  }

  def apply(symbol: Symbol): Big = {
    symbol.dataSize match {
      case IntSize  => currentIntTarget(symbol.index)
      case LongSize => currentLongTarget(symbol.index)
      case BigSize  => currentBigTarget(symbol.index)
    }
  }

  def update(symbol: Symbol, value: Big): Unit = {
    symbol.dataSize match {
      case IntSize  => currentIntTarget(symbol.index) = value.toInt
      case LongSize => currentLongTarget(symbol.index) = value.toLong
      case BigSize  => currentBigTarget(symbol.index) = value
    }
  }
}

object DataStore {
  def apply(numberOfBuffers: Int): DataStore = {
    new DataStore(numberOfBuffers)
  }
}
