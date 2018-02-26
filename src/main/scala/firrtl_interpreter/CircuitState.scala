// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ir._
import firrtl_interpreter.vcd.VCD

import scala.collection.mutable

object CircuitState {
  def apply(dependencyGraph: DependencyGraph): CircuitState = {
    val inputPortToValue  = makePortToConcreteValueMap(dependencyGraph, Input)
    val outputPortToValue = makePortToConcreteValueMap(dependencyGraph, Output)
    val registerToValue   = makeRegisterToConcreteValueMap(dependencyGraph)

    val circuitState = new CircuitState(
      inputPortToValue,
      outputPortToValue,
      registerToValue,
      dependencyGraph.memories,
      dependencyGraph.validNames
    )
    circuitState
  }

  def makeRegisterToConcreteValueMap(dependencyGraph: DependencyGraph): mutable.Map[String, Concrete] = {
    mutable.Map(dependencyGraph.registerNames.map { name =>
      name -> TypeInstanceFactory(dependencyGraph.getType(name))
    }.toSeq:_*)
  }

  def makePortToConcreteValueMap(
                                  dependencyGraph: DependencyGraph,
                                  direction: Direction): mutable.Map[String, Concrete] = {
    mutable.Map(dependencyGraph.module.ports.filter(_.direction == direction).map { port =>
      port.name -> TypeInstanceFactory(port.tpe)
    }: _*)
  }
}

/**
  * Holds the state of the circuit at a particular time
  * State is kept for input, output and registers
  *
  * @param inputPorts  a map to current concrete value
  * @param outputPorts a map to current concrete value
  * @param registers   a map to current concrete value
  */
case class CircuitState(
                    inputPorts:  mutable.Map[String, Concrete],
                    outputPorts: mutable.Map[String, Concrete],
                    registers:   mutable.Map[String, Concrete],
                    memories:    mutable.Map[String, Memory],
                    validNames:  mutable.HashSet[String]) {
  val nextRegisters = new mutable.HashMap[String, Concrete]()
  val ephemera      = new mutable.HashMap[String, Concrete]()
  val rhsOutputs    = new mutable.HashSet[String] // used to see if output has been computed as rhs

  var nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)

  var stateCounter: Int  = 0
  var isStale: Boolean   = true
  var clockHigh: Boolean = false

  var vcdLoggerOption = Option.empty[VCD]
  var vcdOutputFileName = ""

  /**
    * Clone this circuitState.
    * @note This does not yet clone memories.  It merely copies them
    * @return  a cloned version of this state
    */
  override def clone: CircuitState = {
    val newState = CircuitState(
      inputPorts = mutable.Map[String, Concrete]() ++ inputPorts,
      outputPorts = mutable.Map[String, Concrete]() ++ outputPorts,
      registers = mutable.Map[String, Concrete]() ++ registers,
      memories = mutable.Map[String, Memory]() ++ memories,
      validNames = validNames
    )
    newState.nextRegisters ++= nextRegisters
    newState.ephemera ++= ephemera
    newState.rhsOutputs ++= rhsOutputs
    newState.stateCounter = stateCounter
    newState.isStale = isStale
    newState.clockHigh = clockHigh
    newState
  }
  def makeVCDLogger(
      dependencyGraph: DependencyGraph,
      circuitState: CircuitState,
      fileName: String = "out.firrtl_interpreter.vcd",
      showUnderscored: Boolean = false): Unit = {

    val vcd = VCD(dependencyGraph.circuit.main)
    vcdLoggerOption = Some(vcd)
    vcdOutputFileName = fileName

    for(instanceName <- dependencyGraph.instanceNames.keys) {
      if(instanceName != dependencyGraph.circuit.main) {
        vcd.scopeRoot.addScope(instanceName)
      }
    }

    vcd.timeStamp = -1
    for((name, concreteValue) <- circuitState.nameToConcreteValue) {
        vcd.wireChanged(name, concreteValue.value, concreteValue.width, concreteValue.poisoned)
    }
    vcd.timeStamp = 0
  }
  def writeVCD(): Unit = {
    vcdLoggerOption.foreach { _.write(vcdOutputFileName) }
  }
  def disableVCD(): Unit = {
    vcdLoggerOption.foreach { vcd =>
      vcd.write(vcdOutputFileName)
    }
    vcdLoggerOption = None
    vcdOutputFileName = ""
  }

  /**
    * in order to compute dependencies, ephemera must be clear and their
    * associated values cleared
    */
  def prepareForDependencyResolution(): Unit = {
    nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)
    ephemera.clear()
    rhsOutputs.clear()
  }

  def vcdRaiseClock(): Unit = {
    if(! clockHigh) {
      vcdLoggerOption.foreach { vcd =>
        vcd.raiseClock()
      }
      clockHigh = true
    }
  }
  def vcdLowerClock(): Unit = {
    if(clockHigh) {
      vcdLoggerOption.foreach { vcd =>
        vcd.lowerClock()
      }
      clockHigh = false
    }
  }
  def vcdWireChangedwire(key: String, concrete: Concrete): Unit = {
    vcdLoggerOption.foreach { _.wireChanged(key, concrete.value, concrete.width, concrete.poisoned) }
  }
  /**
    * prepare this cycle
    * advance registers
    * clear wire values
    * cycle all memories
    */
  def cycle(): Unit = {
    registers.keys.foreach { key =>
      val nextValue = nextRegisters(key)
      vcdWireChangedwire(key, nextValue)
      registers(key) = nextValue
    }

    cycleMemories()

    nameToConcreteValue = mutable.HashMap((inputPorts ++ outputPorts ++ registers).toSeq:_*)
    isStale = true

    stateCounter += 1
  }
  def cycleMemories(): Unit = {
    memories.values.foreach { memory => memory.cycle() }
  }

  def setValue(key: String, concreteValue: Concrete, registerPoke: Boolean = false): Concrete = {
    if(isInput(key)) {
      inputPorts(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
      vcdWireChangedwire(key, concreteValue)
    }
    else if(isOutput(key)) {
      outputPorts(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
      vcdWireChangedwire(key, concreteValue)
    }
    else if(registers.contains(key)) {
      if(registerPoke) {
        registers(key) = concreteValue
      }
      else {
        nextRegisters(key) = concreteValue
        // registers are logged to VCD during clock postive edge in #cycle()
      }
    }
    else if(isMemory(key)) {
//      println(s"Updating memory interface $key => $concreteValue")
      key match {
        case Memory.KeyPattern(memoryName, _, _) => memories(memoryName).setValue(key, concreteValue)
        case _ =>
          throw new InterpreterException(s"Error:failed memory($key).setValue($key, $concreteValue)")
      }
    }
    else if(validNames.contains(key)) {
      ephemera(key) = concreteValue
      nameToConcreteValue(key) = concreteValue
      vcdWireChangedwire(key, concreteValue)
    }
    else {
      throw InterpreterException(s"Error: setValue($key, $concreteValue) $key is not an element of this circuit")
    }
    isStale = true
    concreteValue
  }

  def getValue(key: String): Option[Concrete] = {
    nameToConcreteValue.get(key) match {
      case Some(value) => Some(value)
      case _=>
        key match {
          case Memory.KeyPattern(memoryName, _, _) =>
            if(memories.contains(memoryName)) {
              Some(memories(memoryName).getValue(key))
            }
            else {
              None
            }
          case _ => None
        }
    }
  }

  def isInput(key: String): Boolean = inputPorts.contains(key)
  def isOutput(key: String): Boolean = outputPorts.contains(key)
  def isRegister(key: String): Boolean = registers.contains(key)
  def isEphemera(key:String): Boolean = {
    ! (isInput(key) || isOutput(key) || isRegister(key))
  }
  def isMemory(key: String): Boolean = {
    val memKey = Memory.memoryKey(key)
    memories.contains(memKey)
  }

  /**
    * prints a human readable version of the state,
    *
    * @param dense if true puts input, output and registers on one line each
    * @return
    */
  def prettyString(dense: Boolean = true): String = {
    val (prefix, separator, postfix) = if(dense) (": ", ", ", "") else (":\n  ", "\n  ", "")
    def showConcreteValues(msg: String, m: Map[String, Concrete]): String = {
      m.keys.toSeq.sorted.map { key =>
        s"$key=${m(key).showValue}"
      }.mkString(msg + prefix, separator, postfix)
    }
    s"""
       |CircuitState $stateCounter (${if(isStale) "STALE" else "FRESH"})
       |${showConcreteValues("Inputs", inputPorts.toMap)}
       |${showConcreteValues("Outputs", outputPorts.toMap)}
       |${showConcreteValues("Registers      ", registers.toMap)}
       |${showConcreteValues("FutureRegisters", nextRegisters.toMap)}
       |${showConcreteValues("Ephemera", ephemera.toMap)}
       |Memories${memories.values.mkString("\n", "\n  ", "")}""".stripMargin
  }
}
