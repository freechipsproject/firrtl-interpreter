// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter.InterpreterException

import scala.collection.mutable

class Scheduler(dataStore: DataStore, symbolTable: SymbolTable) {
  var combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]
  val bufferAdvanceAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]

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

  def scheduleCopy(symbol: Symbol): Unit = {
    val assigner = symbol.dataSize match {
      case IntSize => dataStore.AssignInt(symbol.index, dataStore.GetInt(symbol.index).apply)
      //      case LongSize => dataStore.AssignLong(symbol.index, dataStore.GetLong(symbol.index).apply)
      case BigSize => dataStore.AssignBig(symbol.index, dataStore.GetBig(symbol.index).apply)
    }
    bufferAdvanceAssigns += assigner
  }

  def executeCombinational(): Unit = {
    combinationalAssigns.foreach {
      assign => assign()
    }
  }

  def executeBufferAdvances(): Unit = {
    bufferAdvanceAssigns.foreach {
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

  def sortCombinationalAssigns: Unit = {
    combinationalAssigns = combinationalAssigns.sortBy {
      case assign: dataStore.AssignInt =>
        symbolTable.sortKey(IntSize, assign.index)
      case assign: dataStore.AssignLong =>
        symbolTable.sortKey(LongSize, assign.index)
      case assign: dataStore.AssignBig =>
        symbolTable.sortKey(BigSize, assign.index)
      case assigner =>
        throw InterpreterException(s"unknown assigner found in sort combinational assigns $assigner")
    }
  }

  def render: String = {
    s"combinational assigns (${combinationalAssigns.size}\n" +
    combinationalAssigns.map { assigner =>
      symbolTable(dataStore, assigner).render
    }.mkString("\n")
  }
}

object Scheduler {
  def apply(dataStore: DataStore, symbolTable: SymbolTable): Scheduler = new Scheduler(dataStore, symbolTable)
}
