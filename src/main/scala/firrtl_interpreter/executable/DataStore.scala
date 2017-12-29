// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter.vcd.VCD
import firrtl_interpreter.{BlackBoxImplementation, InterpreterException}

import scala.collection.mutable

/**
  * Creates a data store for the three underlying data types.
  * The numberOfBuffers is used to control the ability to rollback execution.
  * The meaning of the values of each slot must be maintained outside of this class.
  * This class only supports (2 ** 31) - 1 of any ints, longs or bigs.
  *
  * @param numberOfBuffers Number of buffers
  */
class DataStore(val numberOfBuffers: Int, optimizationLevel: Int = 0) {
  assert(numberOfBuffers > 0, s"DataStore: numberOfBuffers $numberOfBuffers must be > 0")

  private val nextIndexFor = new mutable.HashMap[DataSize, Int]
  nextIndexFor(IntSize)  = 0
  nextIndexFor(LongSize) = 0
  nextIndexFor(BigSize)  = 0

  def numberOfInts: Int  = nextIndexFor(IntSize)
  def numberOfLongs: Int = nextIndexFor(LongSize)
  def numberOfBigs: Int  = nextIndexFor(BigSize)

  val symbolToAssigner: mutable.HashMap[Symbol, Assigner] = new mutable.HashMap()

  var vcdOption: Option[VCD] = None

  def vcdUpdate(symbol: Symbol, value: BigInt): Unit = {
    if(vcdOption.isDefined) {
      vcdOption.get.wireChanged(symbol.name, value, width = symbol.bitWidth)
    }
  }
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
  var previousBigArray:  Array[Big]  = bigData(previousBufferIndex)

  def allocateBuffers(): Unit = {
    intData  = Array.fill(numberOfBuffers, numberOfInts)(0)
    longData = Array.fill(numberOfBuffers, numberOfLongs)(0L)
    bigData  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

    currentIntArray  = intData(currentBufferIndex)
    currentLongArray = longData(currentBufferIndex)
    currentBigArray  = bigData(currentBufferIndex)
    previousIntArray  = intData(previousBufferIndex)
    previousLongArray = longData(previousBufferIndex)
    previousBigArray  = bigData(previousBufferIndex)
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
      previousBigArray  = bigData(previousBufferIndex)

      for(i <- currentIntArray.indices)  { currentIntArray(i)  = previousIntArray(i) }
      for(i <- currentLongArray.indices) { currentLongArray(i) = previousLongArray(i) }
      for(i <- currentBigArray.indices)  { currentBigArray(i)  = previousBigArray(i) }
    }
  }

  case class GetInt(index: Int) extends IntExpressionResult {
    def apply(): Int = currentIntArray(index)
  }

  case class AssignInt(symbol: Symbol, expression: FuncInt) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {currentIntArray(index) = expression() }

    def runFull(): Unit = {
      val value = expression()
      if(verboseAssign) {
        val showValue = symbol.normalize(value)
        println(s"${symbol.name} <= $showValue")
      }
      currentIntArray(index) = value
      vcdUpdate(symbol, value)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  case class GetLong(index: Int) extends LongExpressionResult {
    def apply(): Long = currentLongArray(index)
  }

  case class AssignLong(symbol: Symbol, expression: FuncLong) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      currentLongArray(index) = expression()
    }

    def runFull(): Unit = {
      val value = expression()
      if(verboseAssign) {
        val showValue = symbol.normalize(value)
        println(s"${symbol.name} <= $showValue")
      }
      currentLongArray(index) = value
      vcdUpdate(symbol, value)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) {
        runLean _
      } else {
        runFull _
      }
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  case class GetBig(index: Int) extends BigExpressionResult {
    def apply(): Big = currentBigArray(index)
  }

  case class AssignBig(symbol: Symbol, expression: FuncBig) extends Assigner {
    val index: Int = symbol.index

    def runLean(): Unit = {
      currentBigArray(index) = expression()
    }
    def runFull(): Unit = {
      val value = expression()
      if(verboseAssign) {
        val showValue = symbol.normalize(value)
        println(s"${symbol.name} <= $showValue")
      }
      currentBigArray(index) = value
      vcdUpdate(symbol, value)
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if(optimizationLevel == 0) runFull _ else runLean _
  }

  /** for memory implementations */
  case class GetIntIndirect(
                             memorySymbol: Symbol,
                             getMemoryIndex: FuncInt,
                             enable: FuncInt
                           ) extends IntExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Int = {
      currentIntArray(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class GetLongIndirect(
                             memorySymbol: Symbol,
                             getMemoryIndex: FuncInt,
                             enable: FuncInt
                           ) extends LongExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Long = {
      currentLongArray(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class GetBigIndirect(
                             memorySymbol: Symbol,
                             getMemoryIndex: FuncInt,
                             enable: FuncInt
                           ) extends BigExpressionResult {
    val memoryLocation: Int = memorySymbol.index
    def apply(): Big = {
      currentBigArray(memoryLocation + (getMemoryIndex() % memorySymbol.slots))
    }
  }

  case class AssignIntIndirect(
                               symbol: Symbol,
                               memorySymbol: Symbol,
                               getMemoryIndex: FuncInt,
                               enable: FuncInt,
                               expression: FuncInt
                              ) extends Assigner {
    val index: Int = memorySymbol.index

    def runLean(): Unit = {
      if(enable() > 0) {
        currentIntArray(index + getMemoryIndex.apply()) = expression()
      }
    }

    def runFull(): Unit = {
      if(enable() > 0) {
        val value = expression()
        if(verboseAssign) {
          println(s"${symbol.name}(${getMemoryIndex.apply()}) <= $value")
        }
        currentIntArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = value
        vcdUpdate(symbol, value)
      }
      else {
        if(verboseAssign) {
          println(s"${symbol.name}(${(getMemoryIndex.apply() % memorySymbol.slots)}) <= NOT ENABLED")
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if (optimizationLevel == 0) runFull _ else runLean _
  }

  case class AssignLongIndirect(
                               symbol: Symbol,
                               memorySymbol: Symbol,
                               getMemoryIndex: FuncInt,
                               enable: FuncInt,
                               expression: FuncLong
                              ) extends Assigner {
    val index: Int = memorySymbol.index

    def runLean(): Unit = {
      if(enable() > 0) {
        currentLongArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = expression()
      }
    }

    def runFull(): Unit = {
      if(enable() > 0) {
        val value = expression()
        if(verboseAssign) {
          println(s"${symbol.name}(${(getMemoryIndex.apply() % memorySymbol.slots)}) <= $value")
        }
        currentLongArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = value
        vcdUpdate(symbol, value)
      }
      else {
        if(verboseAssign) {
          println(s"${symbol.name}(${(getMemoryIndex.apply() % memorySymbol.slots)}) <= NOT ENABLED")
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if (optimizationLevel == 0) runFull _ else runLean _
  }

  case class AssignBigIndirect(
                                 symbol: Symbol,
                                 memorySymbol: Symbol,
                                 getMemoryIndex: FuncInt,
                                 enable: FuncInt,
                                 expression: FuncBig
                               ) extends Assigner {
    val index: Int = memorySymbol.index

    def runLean(): Unit = {
      if(enable() > 0) {
        currentBigArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = expression()
      }
    }

    def runFull(): Unit = {
      if(enable() > 0) {
        val value = expression()
        if(verboseAssign) {
          println(s"${symbol.name}(${(getMemoryIndex.apply() % memorySymbol.slots)}) <= $value")
        }
        currentBigArray(index + (getMemoryIndex.apply() % memorySymbol.slots)) = value
        vcdUpdate(symbol, value)
      }
      else {
        if(verboseAssign) {
          println(s"${symbol.name}(${(getMemoryIndex.apply() % memorySymbol.slots)}) <= NOT ENABLED")
        }
      }
    }

    override def setLeanMode(isLean: Boolean): Unit = {
      run = if(isLean) runLean _ else runFull _
    }
    var run: FuncUnit = if (optimizationLevel == 0) runFull _ else runLean _
  }

  case class BlackBoxShim(
      unexpandedName: String,
      outputName:     Symbol,
      inputs:         Seq[Symbol],
      implementation: BlackBoxImplementation
  )
  extends BigExpressionResult {

    val dataStore: DataStore = DataStore.this

    def apply(): Big = {
      val inputValues = inputs.map { input => dataStore(input) }
      val bigInt = implementation.execute(inputValues, outputName.firrtlType, unexpandedName)
      bigInt
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

  def setValueAtIndex(dataSize: DataSize, index: Int, value: Big): Unit = {
    dataSize match {
      case IntSize  => currentIntArray(index)  = value.toInt
      case LongSize => currentLongArray(index) = value.toLong
      case BigSize  => currentBigArray(index)  = value
    }
  }

  def getValueAtIndex(dataSize: DataSize, index: Int): BigInt = {
    dataSize match {
      case IntSize  => currentIntArray(index)
      case LongSize => currentLongArray(index)
      case BigSize  => currentBigArray(index)
    }
  }
}

object DataStore {
  def apply(numberOfBuffers: Int, optimizationLevel: Int = 1): DataStore = {
    new DataStore(numberOfBuffers, optimizationLevel)
  }
}
