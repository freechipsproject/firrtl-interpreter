// See LICENSE for license details.

package firrtl_interpreter

import scala.collection.mutable.ArrayBuffer

class ExternalModule {

}

case class BlackBoxOutput(name: String, implementation: BlackBoxImplementation) {
  def execute: Concrete = {
    implementation.getOutput(name)
  }

}

class BlackBoxImplementation {
  def getOutput(outputName: String): Concrete = {
    ConcreteUInt(-1, 2)
  }
  def step: Unit = {

  }
}

abstract class BlackBoxFactory {
  val boxes = new ArrayBuffer[BlackBoxImplementation]
  def add(blackBox: BlackBoxImplementation): BlackBoxImplementation = {
    boxes += blackBox
    blackBox
  }
  def constructor(info: String): BlackBoxImplementation
  def appliesTo(blackBoxName: String): Boolean

  def build(info: String): BlackBoxImplementation = {
    add(constructor(info))
  }
  def step: Unit = {
    boxes.foreach { box => box.step }
  }
}