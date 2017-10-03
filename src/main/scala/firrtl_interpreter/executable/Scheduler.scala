// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable

class Scheduler(dataStore: DataStore, symbolTable: SymbolTable) {
  val combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]
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

  def sortCombinational(dependencyTracker: DependencyTracker): Unit = {
    def compare(a: Assigner, b: Assigner): Boolean = {
      (a, b) match {
        case (aa: dataStore.AssignInt, bb: dataStore.AssignInt) =>
          dependencyTracker.symbolSortKey(symbolTable(IntSize, aa.index).name) <
            dependencyTracker.symbolSortKey(symbolTable(IntSize, bb.index).name)
        case _ => false
      }
    }
  }
}

object Scheduler {
  def apply(dataStore: DataStore, symbolTable: SymbolTable): Scheduler = new Scheduler(dataStore, symbolTable)
}
