// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ir.{Type, Expression, Width}

import scala.collection._

class ExternalModule {

}

case class BlackBoxOutput(name: String,
                          implementation: BlackBoxImplementation,
                          dependentInputs: Seq[String],
                          tpe: Type
                         ) extends Expression {
  def mapExpr(f: Expression => Expression): Expression = f(this)
  def mapType(f: Type => Type): Expression = this
  def mapWidth(f: Width => Width): Expression = this
  def execute(inputValues: Seq[Concrete]): Concrete = {
    implementation.execute(inputValues, tpe: Type)
  }
  def serialize: String = s"BlackBoxOutput($name,$tpe)"
}

abstract class BlackBoxImplementation {
  def name: String
  def fullName(componentName: String): String = s"$name.$componentName"
  def execute(inputValues: Seq[Concrete], tpe: Type): Concrete
  def cycle(): Unit

  def outputDependencies(outputName: String): Seq[String]
}

abstract class BlackBoxFactory {
  def boxes: mutable.HashMap[String, BlackBoxImplementation] = new mutable.HashMap[String, BlackBoxImplementation]

  def add(blackBox: BlackBoxImplementation): BlackBoxImplementation = {
    boxes(blackBox.name) = blackBox
    blackBox
  }
  def createInstance(instanceName: String, blackBoxName: String): Option[BlackBoxImplementation]

  def cycle(): Unit = {
    boxes.values.foreach { box => box.cycle() }
  }
}
