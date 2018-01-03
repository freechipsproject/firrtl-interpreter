// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable


object RenderHelper {

  implicit class ExpressionHelper(val sc: StringContext) extends AnyVal {
    def expression(args: Any*)(implicit ds: DataStore): ExpressionView = {
      new ExpressionView(ds, sc, args.toSeq)
    }
  }

}

class ExpressionView(val dataStore: DataStore, val sc: StringContext, val args: Seq[Any])

case class SymbolAtDepth(symbol: Symbol, depth: Int)


/**
  * This class answers the question why does the given symbol have a particular value
  *
  * @param dataStore        current state
  * @param symbolTable      the symbol table
  * @param expressionViews  expression information
  */
class ExpressionViewRenderer(
    dataStore: DataStore,
    symbolTable: SymbolTable,
    expressionViews: Map[Symbol, ExpressionView]
) {

  private def order(symbolAtDepth: SymbolAtDepth) = symbolAtDepth.depth

  private val symbolsToDo = new mutable.PriorityQueue[SymbolAtDepth]()(Ordering.by(order))
  private val symbolsSeen = new mutable.HashSet[Symbol]()

  def renderInternal(): String = {
    val builder = new StringBuilder()

    def renderView(view: ExpressionView, depth: Int = 1): String = {
      val builder = new StringBuilder()

      val sc = view.sc
      val args = view.args

      builder ++= sc.parts.head
      val argStrings = args.map {
        case s: Symbol =>
          if(! (
            symbolTable.isRegister(s.name) ||
              symbolTable.inputPortsNames.contains(s.name) ||
              symbolsSeen.contains(s)
            )) {
            symbolsToDo.enqueue(SymbolAtDepth(s, depth + 1))
          }
          symbolsSeen += s
          s"${s.name}:${dataStore.earlierValue(s, 1)}"

        case subView: ExpressionView =>
          renderView(subView, depth + 1)

        case x => x.toString
      }

      argStrings.zip(sc.parts.tail).foreach { case (s1, s2) =>
        builder ++= s1
        builder ++= s2
      }
      builder.toString()
    }

    while (symbolsToDo.nonEmpty) {
      val symbolAtDepth = symbolsToDo.dequeue()
      expressionViews.get(symbolAtDepth.symbol).foreach { view =>
        builder ++= "  " * symbolAtDepth.depth
        builder ++= s"${symbolAtDepth.symbol.name} <= "
        builder ++= renderView(view)
        builder ++= "\n"
      }
    }

    val result = builder.toString()
    result
  }

  def render(symbol: Symbol): String = {

    symbolsToDo.enqueue(SymbolAtDepth(symbol, 0))
    renderInternal()
  }
}





