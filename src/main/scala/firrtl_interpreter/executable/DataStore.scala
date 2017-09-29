// See LICENSE for license details.

package firrtl_interpreter.executable

/**
  * Creates a data store for the three underlying data types.
  * The numberOfBuffers is used to control the ability to rollback execution.
  * The meaning of the values of each slot must be maintained outside of this class.
  * This class only supports (2 ** 31) - 1 of any ints, longs or bigs.
  * @param numberOfInts    Number of Ints needed
  * @param numberOfLongs   Number of Longs needed
  * @param numberOfBigs    Number of Bigs needed
  * @param numberOfBuffers Number of buffers
  */
class DataStore(numberOfInts: Int, numberOfLongs: Int, numberOfBigs: Int, numberOfBuffers: Int) {
  assert(numberOfBuffers > 0, s"DataStore: numberOfBuffers $numberOfBuffers must be > 0")
  val intData:  Array[Array[Int]]  = Array.fill(numberOfBuffers, numberOfInts)(0)
  val longData: Array[Array[Long]] = Array.fill(numberOfBuffers, numberOfLongs)(0L)
  val bigData:  Array[Array[Big]]  = Array.fill(numberOfBuffers, numberOfBigs)(Big(0))

  private var targetBufferIndex = if(numberOfBuffers > 1) 1 else 0
  private var sourceBufferIndex = 0

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
    }
  }
}

object DataStore {
  def apply(numberOfInts: Int, numberOfLongs: Int, numberOfBigs: Int, numberOfBuffers: Int): DataStore = {
    new DataStore(numberOfInts, numberOfLongs, numberOfBigs, numberOfBuffers)
  }
  def marshall(): String = { ??? }
  def unmarshall(dataString: String): Unit = { ??? }
}
