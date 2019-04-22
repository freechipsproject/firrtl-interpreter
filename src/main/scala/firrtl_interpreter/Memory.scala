// See LICENSE for license details.
package firrtl_interpreter

import firrtl.ir._

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/**
  * provides a black box implementation of a circuit memory presenting  read, write and read/write interfaces
  *
  * Assumptions:
  * Zero read_latency latches data straight to memory(address): IGNORES enable
  *
  * @param info source level information
  * @param name the name of this memory
  * @param dataType type of each memory element
  * @param depth number of elements
  * @param writeLatency how many cycles before write happens
  * @param readLatency how many cycles before read happens
  * @param readers a list of named reader ports
  * @param writers a list of named writer ports
  * @param readWriters list of named read/write ports
  * @param readUnderWrite behavior
  */
// TODO: Should enable choke if fed values other than zero or 1 or things wider than 1
class Memory(
              val info: Info,
              val name: String,
              val dataType: Type,
              val depth: Int,
              val writeLatency: Int,
              val readLatency: Int,
              val readers: Seq[String],
              val writers: Seq[String],
              val readWriters: Seq[String],
              val readUnderWrite: String
            ) extends SimpleLogger {
  import Memory._

  val dataWidth: Int    = typeToWidth(dataType)
  val addressWidth: Int = requiredBitsForUInt(depth)
  val bigDepth: BigInt  = BigInt(depth)
  var moduloIndex: Boolean  = true

  val maxMemoryInDefaultDisplay = 20

  assert(writeLatency == 1, s"Interpreter memory $name write latency $writeLatency not supported, must be 1")
  assert(readLatency <  2,  s"Interpreter memory $name read latency $readLatency not supported, must be 0 or 1")
  assert(readLatency >= 0,  s"Interpreter memory $name read latency $readLatency not supported, must be 0 or 1")

  val ports: Map[String, MemoryPort] = {
    (
      readers.map     { portName => portName -> ReadPort(portName, readLatency) } ++
      writers.map     { portName => portName -> WritePort(portName, writeLatency) } ++
      readWriters.map { portName => portName -> ReadWritePort(portName) }
    ).toMap
  }
  val writePorts:     Array[WritePort]     = writers.map(writer => ports(writer).asInstanceOf[WritePort]).toArray
  val readPorts:      Array[ReadPort]      = readers.map(reader => ports(reader).asInstanceOf[ReadPort]).toArray
  val readWritePorts: Array[ReadWritePort] = readWriters.map { readWriter =>
    ports(readWriter).asInstanceOf[ReadWritePort]
  }.toArray

  /**
    * wrap underlying data storage array so indexing is automatically constrained at depth
    */
  class DataStore {
    val underlyingData: Array[Concrete] = Array.fill(depth)(Concrete(dataType))
    def apply(index: Int): Concrete = underlyingData(index % depth)
    def update(index: Int, value: Concrete): Unit = {
      underlyingData(index % depth) = value
    }
    def length: Int = underlyingData.length
  }

  val dataStore = new DataStore

  def getValue(key: String): Concrete = {
    key match {
      case Memory.KeyPattern(memoryName, portName, fieldName) =>
        log(s"In memory($memoryName).port($portName).getValue($fieldName) => ${ports(portName).getValue(fieldName)})")
        ports(portName).getValue(fieldName)
      case _ =>
        throw new InterpreterException(s"Error: bad memory($key).getValue($key)")
    }
  }

  /**
    * delegate the concrete value to a port
    * various actions may ensue depending on the
    *
    * @param key full ram.port.field specifier
    * @param concreteValue current value
    */
  def setValue(key: String, concreteValue: Concrete): Unit = {
    key match {
      case KeyPattern(memoryName, portName, fieldName) =>
        assert(name == memoryName, s"Error:bad dispatch memory($name).setValue($key, $concreteValue)")
        log(s"In memory($memoryName).port($portName).setValue($fieldName, $concreteValue)")
        ports(portName) match {
          case p: ReadPort      => p.setValue(fieldName, concreteValue)
          case p: WritePort     => p.setValue(fieldName, concreteValue)
          case p: ReadWritePort => p.setValue(fieldName, concreteValue)
        }
    }
  }

  def forceWrite(offset: Int, value: BigInt): Unit = {
    val concrete = TypeInstanceFactory(dataType, value)
    if(offset > depth) {
      throw InterpreterException(
        s"Memory ${this.name}.forceWrite(offset = $offset, value = $value) offset too big, max is $depth")
    }
    dataStore(offset) = concrete
  }

  /**
    * used to inform this memory that a cycle has passed
    */
  def cycle(): Unit = {
    for(writer <- writePorts) writer.cycle()
    for(reader <- readPorts) reader.cycle()
    for(readWriter <- readWritePorts) readWriter.cycle()

    log(s"memory cycled $toString")
  }

  def getAllFieldDependencies: Seq[String] = {
    (readPorts ++ writePorts ++ readWritePorts).flatMap { port: MemoryPort => port.fieldDependencies}
  }
  def getAllOutputFields: Seq[(String, Seq[String])] = {
    (readPorts ++ writePorts ++ readWritePorts).map { port: MemoryPort =>
      (port.outputFieldName, port.fieldDependencies)
    }
  }

  override def toString: String = {
    s"memory $name" +
    readPorts.mkString(" rp:", ",", "") +
    writePorts.mkString(" wp:", ",", " ") +
    readWritePorts.mkString(" rwp:[", ",", "] mem: ") +
      (0 until depth.min(maxMemoryInDefaultDisplay)).map(a => dataStore(a).value).mkString(",")
  }

  trait PipeLineElement

  abstract class MemoryPort {
    val portName: String
    val latency: Int
    var enable: Boolean     = false
    var clock: Int          = 0
    var address: Int        = 0
    var data: Concrete      = Concrete.poisonedUInt(dataWidth)

    def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      fieldName match {
        case "en"      => enable  = concreteValue.value > Big0
        case "clk"     => clock   = concreteValue.value.toInt
        case "addr"    => address = concreteValue.value.toInt
        case "data"    => data    = concreteValue
        case _  =>
          throw new Exception(s"error:bad field specifier memory $fullName.setValue($fieldName, $concreteValue)")
      }
      log(s"port is now en $enable addr $address data $data")
    }
    def getValue(fieldName: String): Concrete = {
      fieldName match {
        case "en"      => ConcreteUInt(boolToBigInt(enable), 1)
        case "clk"     => ConcreteClock(clock)
        case "addr"    => ConcreteUInt(address, addressWidth)
        case "data"    => data
        case _  =>
          throw new Exception(s"error:bad field specifier memory $fullName.getValue($fieldName)")
      }
    }
    def fieldDependencies: Seq[String]
    val outputFieldName: String = s"$name.$portName.data"
    val fullName: String = s"memory $name.$portName"
  }

  /**
    * implements a read port with memory defined latency
    *
    * @param portName name of this reader
    * @param latency  the number of cycles between port and memory
    */
  case class ReadPort(portName: String, latency: Int) extends MemoryPort {
    val fieldDependencies: Seq[String] = Seq("en", "addr").map { fieldName => s"$name.$portName.$fieldName"}

    case class ReadPipeLineElement(readPipeLineData: Concrete) {
      override def toString: String = s"[${readPipeLineData.value}]"
    }
    val pipeLine : ArrayBuffer[ReadPipeLineElement] = {
      ArrayBuffer.fill(latency)(ReadPipeLineElement(Concrete.poisonedUInt(dataWidth)))
    }

    override def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      super.setValue(fieldName, concreteValue)
      inputHasChanged()
      log(s"port is now en $enable addr $address data $data")
    }
    def inputHasChanged(): Unit = {
      if(latency == 0) {
        data = dataStore(address)
      }
      else {
        pipeLine(0) = ReadPipeLineElement(if(enable) dataStore(address) else Concrete.poisonedUInt(dataWidth))
      }
    }
    def cycle(): Unit = {
      if(latency > 0) {
        data = pipeLine.remove(0).readPipeLineData
        pipeLine += ReadPipeLineElement(if(enable) dataStore(address) else Concrete.poisonedUInt(dataWidth))
      }
      else {
        data = if(enable) dataStore(address) else Concrete.poisonedUInt(dataWidth)
      }
    }
    override def toString: String = {
      s"[$enable:$address:${data.value}" +
        (if(latency>0) pipeLine.mkString(" pl:", ",", "") else "]")
    }
  }

  /**
    * implements a write port with memory defined latency
    *
    * @param portName name of this writer
    * @param latency  the number of cycles between port and memory
    */
  case class WritePort(portName: String, latency: Int) extends MemoryPort {
    var mask: Concrete           = ConcreteUInt(0, 1)

    case class WritePipeLineElement(enable: Boolean, address: Int, data: Concrete, mask: Concrete) {
      override def toString: String = s"[$enable:$address:${data.value}:${mask.value}]"
    }
    val pipeLine : ArrayBuffer[WritePipeLineElement] = {
      ArrayBuffer.empty[WritePipeLineElement] ++
        Array.fill(latency)(elementFromSnapshot)
    }

    def elementFromSnapshot: WritePipeLineElement = {
      WritePipeLineElement(enable, address, data, mask)
    }

    def inputHasChanged(): Unit = {
      if(latency > 0) {
        val newElement = elementFromSnapshot
        log(s"memory $fullName input changed $newElement")
        pipeLine(0) =  newElement
      }
    }
    override def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      fieldName match {
        case "mask"    => mask = concreteValue
        case "wdata"   => data = concreteValue
        case _         => super.setValue(fieldName, concreteValue)
      }
      inputHasChanged()
    }
    override def getValue(fieldName: String): Concrete = {
      fieldName match {
        case "mask"    => mask
        case _         => super.getValue(fieldName)
      }
    }
    def cycle(): Unit = {
      if(latency > 0) {
        val element = pipeLine.remove(0)
        if (element.enable && element.mask.value > 0) {
          log(s"memory $fullName cycle element is $element, executed")
          dataStore(element.address) = element.data
        }
        else {
          log(s"memory $fullName cycle element is $element, REJECTED")
        }
        pipeLine += elementFromSnapshot
      }
      else {
        // combinational write
        dataStore(address) = data
      }
    }
    val fieldDependencies: Seq[String] = Seq("en", "addr", "data", "mask").map { fieldName =>
      s"$name.$portName.$fieldName"
    }
    override def toString: String = {
      s"[$enable:$address:${data.value},${mask.value}" +
        (if(latency>0) pipeLine.mkString(" pl:", ",", "]") else "]")
    }
  }

  case class ReadWritePort(portName: String) extends MemoryPort {
    val latency:   Int           = readLatency
    var writeMode: Boolean       = false
    var readData:  Concrete      = Concrete.poisonedUInt(dataWidth)
    var mask:      Concrete      = ConcreteUInt(0, dataWidth)

    case class ReadPipeLineElement(readPipeLineData: Concrete) {
      override def toString: String = s"[${readPipeLineData.value}]"
    }
    val readPipeLine : ArrayBuffer[ReadPipeLineElement] = {
      ArrayBuffer.fill(readLatency)(ReadPipeLineElement(Concrete.poisonedUInt(dataWidth)))
    }

    case class WritePipeLineElement(enable: Boolean, address: Int, data: Concrete, mask: Concrete) {
      override def toString: String = s"[$enable:$address:${data.value}:${mask.value}]"
    }
    val writePipeLine : ArrayBuffer[WritePipeLineElement] = {
      ArrayBuffer.fill[WritePipeLineElement](writeLatency)(writeElementFromSnapshot)
    }
    def writeElementFromSnapshot: WritePipeLineElement = {
      WritePipeLineElement(enable && writeMode, address, data, mask)
    }
    def inputHasChanged(): Unit = {
      if(writeMode) {
        if (latency > 0) {
          val newElement = writeElementFromSnapshot
          log(s"memory $fullName input changed $newElement")
          writePipeLine(0) = newElement
        }
      }
      else {
        if(latency == 0) {
          readData = dataStore(address)
        }
        else {
          readPipeLine(0) = ReadPipeLineElement(if(enable) dataStore(address) else Concrete.poisonedUInt(dataWidth))
        }
      }
    }

    override def setValue(fieldName: String, concreteValue: Concrete): Unit = {
      fieldName match {
        case "wmode"   => writeMode = concreteValue.value > Big0
        case "wdata"   => data      = concreteValue
        case "wmask"   => mask      = concreteValue
        case "mask"    => mask      = concreteValue
        case _         => super.setValue(fieldName, concreteValue)
      }
      inputHasChanged()
    }
    override def getValue(fieldName: String): Concrete = {
      fieldName match {
        case "wmod"    => ConcreteUInt(boolToBigInt(writeMode), 1)
        case "rdata"   => readData
        case "mask"    => mask
        case _         => super.getValue(fieldName)
      }
    }
    def cycle(): Unit = {
      if(readLatency > 0) {
        readData = readPipeLine.remove(0).readPipeLineData
        readPipeLine += ReadPipeLineElement(if(enable) dataStore(address) else Concrete.poisonedUInt(dataWidth))
      }
      else {
        readData = if(enable) dataStore(address) else Concrete.poisonedUInt(dataWidth)
      }

      if(writeLatency > 0) {
        val element = writePipeLine.remove(0)
        if (element.enable && element.mask.value > 0) {
          log(s"memory $fullName cycle element is $element, executed")
          dataStore(element.address) = element.data
        }
        else {
          log(s"memory $fullName cycle element is $element, REJECTED")
        }
        writePipeLine += writeElementFromSnapshot
      }
      else {
        // combinational write
        dataStore(address) = data
      }


    }
    val fieldDependencies: Seq[String] = Seq("en", "addr", "wdata", "wmask", "wmode").map { fieldName =>
      s"$name.$portName.$fieldName"
    }
    override val outputFieldName = s"$name.$portName.rdata"

    override def toString: String = {
      s"[${if(writeMode)"W" else "R"}:$enable:$address:${data.value}:${readData.value},${mask.value}" +
        (if(readLatency>0) readPipeLine.mkString(" rpl:[", ",", "]")) +
        (if(writeLatency>0) writePipeLine.mkString(" wpl:[", ",", "]")) +
        "]"
    }
  }
}

object Memory {
  def apply(defMemory: DefMemory): Memory = {
    if(defMemory.depth >= BigInt(Int.MaxValue)) {
      throw InterpreterException(s"Interpreter error: memory ${defMemory.depth} is too big")
    }
    new Memory(
      defMemory.info,
      defMemory.name,
      defMemory.dataType,
      defMemory.depth.toInt,
      defMemory.writeLatency,
      defMemory.readLatency,
      defMemory.readers,
      defMemory.writers,
      defMemory.readwriters,
      ""
    )
  }
  def memoryKey(key: String): String = {
    key match {
      case KeyPattern(memKey, _, _) => memKey
      case _ => key
    }
  }
  val KeyPattern: Regex = """(.*)\.([^\.]*)\.([^\.]*)""".r
}