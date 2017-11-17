// See LICENSE for license details.

package firrtl_interpreter.executable

import logger.LazyLogging

import scala.collection.mutable

class Scheduler(val dataStore: DataStore, val symbolTable: SymbolTable) extends LazyLogging {
  var combinationalAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]
  val bufferAdvanceAssigns: mutable.ArrayBuffer[Assigner] = new mutable.ArrayBuffer[Assigner]

  /**
    * associates an ExpressionResult (probably some kind of clock) with a bunch of assignments
    * that happen on leading edge of that expression
    */
  val triggeredAssigns: mutable.HashMap[Symbol, mutable.ArrayBuffer[Assigner]] = {
    new mutable.HashMap[Symbol, mutable.ArrayBuffer[Assigner]] {
      override def default(key: Symbol): mutable.ArrayBuffer[Assigner] = {
        this(key) = new mutable.ArrayBuffer[Assigner]()
        this(key)
      }
    }
  }

  def executeCombinational(): Unit = {
     println(s"Executing combinational assigns")
    combinationalAssigns.foreach { assign =>
      assign.run()
    }
  }

  def executeTriggeredAssigns(symbol: Symbol): Unit = {
    val triggerValue = symbol.dataSize match {
      case IntSize  => dataStore.currentIntArray(symbol.index) > 0
      case LongSize => dataStore.currentLongArray(symbol.index) > 0L
      case BigSize  => dataStore.currentBigArray(symbol.index) > Big(0)
    }
    if(triggerValue) {
      triggeredAssigns(symbol).foreach {
        assign => assign.run()
      }
    }
  }

  def getTriggerExpressions: Iterable[Symbol] = {
    triggeredAssigns.keys
  }

  def sortCombinationalAssigns(): Unit = {
    combinationalAssigns = combinationalAssigns.sortBy { assigner: Assigner =>
      symbolTable.sortKey(assigner.symbol.dataSize, assigner.symbol.index)
    }
  }

  def sortTriggeredAssigns(): Unit = {
    triggeredAssigns.foreach { case (trigger, assigners) =>
      triggeredAssigns(trigger) = assigners.sortBy { assigner: Assigner =>
        symbolTable.sortKey(assigner.symbol.dataSize, assigner.symbol.index)
      }
    }

//    triggeredAssigns.foreach { case (trigger, assigners) =>
//      val assignedSymbols = assigners.map(dataStore.assignerToSymbol(_)).toList.distinct
//      val dependentSymbols = assignedSymbols.flatMap { symbol =>
//        symbolTable.parentsOf.reachableFrom(symbol)
//      }
//    }
  }

  def makeFresh(): Unit = {
    //TODO (chick) make sure dataStore is up to date

  }

  def render: String = {
    s"combinational assigns (${combinationalAssigns.size})\n" +
    combinationalAssigns.map { assigner =>
      symbolTable(dataStore, assigner).render
    }.mkString("\n") + "\n\n" +
    triggeredAssigns.keys.toList.map { key =>
      s"Triggered assigns for $key\n" +
      triggeredAssigns(key).map { assigner =>
        "  " + symbolTable(dataStore, assigner).render
      }.mkString("\n")
    }.mkString("\n")
  }
}

object Scheduler {
  def apply(dataStore: DataStore, symbolTable: SymbolTable): Scheduler = new Scheduler(dataStore, symbolTable)
}
