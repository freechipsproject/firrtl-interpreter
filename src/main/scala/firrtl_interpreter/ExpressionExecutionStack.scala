// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ir.Expression

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object ExpressionExecutionStack {
  val defaultMaxExecutionDepth: Long = 100000
}

class ExpressionExecutionStack(parent: LoFirrtlExpressionEvaluator) {
  val dependencyGraph = parent.dependencyGraph
  var maxExecutionDepth = ExpressionExecutionStack.defaultMaxExecutionDepth
  def allowCombinationalLoops: Boolean = parent.allowCombinationalLoops

  val expressionStack = new ArrayBuffer[StackItem]
  val stackKeys = new mutable.HashSet[String]

  case class StackItem(lhsOpt: Option[String], expression: Expression) {
    override def toString: String = {
      s"${dependencyGraph.addKind(lhsOpt.getOrElse("     "))} -> ${expression.serialize}"
    }
  }

  def stackListing: String = {
    expressionStack.filter(_.lhsOpt.nonEmpty).zipWithIndex.map { case (entry,index) =>
      f"$index%4d $entry"
    }.mkString("\n")
  }

  def push(keyOption: Option[String], expression: Expression): Boolean = {
    var returnValue = true
    expressionStack += StackItem(keyOption, expression)
    if(expressionStack.length > maxExecutionDepth) {
      throw new InterruptedException(s"Expression Stack too deep, max is $maxExecutionDepth")
    }
    keyOption.foreach { expressionKey =>
      if(stackKeys.contains(expressionKey)) {
        if(allowCombinationalLoops) {
          returnValue = false
        }
        else {
          throw new InterpreterException(s"Expression key $expressionKey already in stack")
        }
      }
      stackKeys += expressionKey
    }
    returnValue
  }

  def pop(): StackItem = {
    val lastItem = expressionStack.remove(expressionStack.length-1)
    lastItem.lhsOpt.foreach { key => stackKeys -= key }
    lastItem
  }

  def clear(): Unit = {
    expressionStack.clear()
    stackKeys.clear()
  }
}
