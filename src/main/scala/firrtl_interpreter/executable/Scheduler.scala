// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

class Scheduler {
  val combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]

  /**
    * associates an ExpressionResult (probably some kind of clock) with a bunch of assignments
    * that happen on leading edge of that expression
    */
  val triggeredAssigns: mutable.HashMap[ExpressionResult, mutable.ArrayBuffer[Assigner]] = {
    new mutable.HashMap[ExpressionResult, mutable.ArrayBuffer[Assigner]] {
      override def default(key: ExpressionResult): mutable.ArrayBuffer[Assigner] = {
        this(key) = new mutable.ArrayBuffer[Assigner]()
        this(key)
      }
    }
  }

  def executeCombinational(): Unit = {
    combinationalAssigns.foreach {
      assign => assign()
    }
  }

  def executeTriggeredAssigns(triggerExpression: ExpressionResult): Unit = {
    triggeredAssigns(triggerExpression).foreach {
      assign => assign()
    }
  }

  def getTriggerExpressions: Iterable[ExpressionResult] = {
    triggeredAssigns.keys
  }
}

object Scheduler {
  def apply(): Scheduler = new Scheduler
}
