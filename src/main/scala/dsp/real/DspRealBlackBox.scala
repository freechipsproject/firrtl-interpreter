// See LICENSE for license details.

package dsp.real

import firrtl_interpreter.{ConcreteUInt, Concrete, BlackBoxImplementation, BlackBoxFactory}

class DspRealAdd(name: String) extends BlackBoxImplementation {
  def execute: Concrete = {
    ConcreteUInt(-1, 1)
  }
}

class DspRealFactory extends BlackBoxFactory {
  def appliesTo(blackBoxName: String): Boolean = {
    blackBoxName match {
      case "BBFAdd" => true
      case _ => false
    }
  }
  def constructor(info: String): BlackBoxImplementation = {
    info match {
      case "Add" =>
        new DspRealAdd("RealAdder")
    }
  }
}