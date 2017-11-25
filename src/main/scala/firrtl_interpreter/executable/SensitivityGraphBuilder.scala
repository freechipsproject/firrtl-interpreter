// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.graph.{DiGraph, MutableDiGraph}

class SensitivityGraphBuilder {
  val childrenOf: MutableDiGraph[Symbol] = new MutableDiGraph[Symbol]
  val parentsOf:  MutableDiGraph[Symbol] = new MutableDiGraph[Symbol]

  def addSensitivity(drivingSymbol: Symbol, sensitiveSymbol: Symbol): Unit = {
    if(!childrenOf.contains(drivingSymbol)) childrenOf.addVertex(drivingSymbol)
    if(!childrenOf.contains(sensitiveSymbol)) childrenOf.addVertex(sensitiveSymbol)
    if(!parentsOf.contains(drivingSymbol)) parentsOf.addVertex(drivingSymbol)
    if(!parentsOf.contains(sensitiveSymbol)) parentsOf.addVertex(sensitiveSymbol)

    childrenOf.addPairWithEdge(drivingSymbol, sensitiveSymbol)
    parentsOf.addPairWithEdge(sensitiveSymbol, drivingSymbol)
  }

  def getChildrenOfDiGraph: DiGraph[Symbol] = DiGraph(childrenOf)
  def getParentsOfDiGraph:  DiGraph[Symbol] = DiGraph(parentsOf)

  def orphans(symbolTable: SymbolTable): Seq[Symbol] = {
    parentsOf.getVertices.filter { symbol =>
      parentsOf.reachableFrom(symbol).isEmpty && ! symbolTable.isTopLevelInput(symbol.name)
    }.toSeq
  }
}
