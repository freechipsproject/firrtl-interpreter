// See LICENSE for license details.

package firrtl_interpreter.executable

import scala.collection.mutable


object RenderHelper {

  implicit class ExpressionHelper(val sc: StringContext) extends AnyVal {
    def expression(args: Any*): ExpressionView = {
      new ExpressionView(sc, args.toSeq)
    }
  }

}

class ExpressionView(val sc: StringContext, val args: Seq[Any])

class SymbolAtDepth(val symbol: Symbol, val displayDepth: Int, val lookBackDepth: Int)

object SymbolAtDepth {
  def apply(symbol: Symbol, displayDepth: Int, lookBackDepth: Int): SymbolAtDepth = {
    new SymbolAtDepth(symbol, displayDepth, lookBackDepth)
  }
}


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

  private def order(symbolAtDepth: SymbolAtDepth) = symbolAtDepth.displayDepth

  private val symbolsToDo = new mutable.PriorityQueue[SymbolAtDepth]()(Ordering.by(order))
  private val symbolsSeen = new mutable.HashSet[Symbol]()

  //scalastyle:off cyclomatic.complexity method.length
  private def renderInternal(): String = {
    val builder = new StringBuilder()

    def renderView(view: ExpressionView, displayDepth: Int, lookBackDepth: Int): String = {
      val builder = new StringBuilder()

      val sc = view.sc
      val args = view.args

      /* If the current view is a mux only descend the branch taken based on the mux condition */
      def checkForMux(): Unit = {
        if(sc.parts.head == "Mux(") {
          args.head match {
            case ev: ExpressionView =>
              ev.args.head match {
                case ms: Symbol =>
                  val arg = args.drop(if(dataStore(ms) > 0) 2 else 1).head
                  arg match {
                    case ev2: ExpressionView =>
                      ev2.args.head match {
                        case sss: Symbol =>
                          symbolsSeen += sss
                      }
                    case _ =>
                  }
              }
          }
        }
      }

      checkForMux()

      builder ++= sc.parts.head
      val argStrings = args.map {
        case s: Symbol =>
          if(! (
            symbolTable.isRegister(s.name) ||
              symbolTable.inputPortsNames.contains(s.name) ||
              symbolsSeen.contains(s)
            )) {
            symbolsToDo.enqueue(SymbolAtDepth(s, displayDepth + 1, lookBackDepth))
          }

          symbolsSeen += s

          (if(lookBackDepth > 1) Console.RED else "") +
          s"${s.name}:${dataStore.earlierValue(s, lookBackDepth)}" +
          (if(lookBackDepth > 1) Console.RESET else "")

        case subView: ExpressionView =>
          renderView(subView, displayDepth + 1, lookBackDepth)

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
      val symbol = symbolAtDepth.symbol
      val lookBackDepth = symbolAtDepth.lookBackDepth
      val currentValue = symbol.normalize(dataStore.earlierValue(symbol, lookBackDepth))
      val adjustedLookBackDepth = lookBackDepth + (if(symbolTable.isRegister(symbol.name)) 1 else 0)

      expressionViews.get(symbol).foreach { view =>
        builder ++= "  " * symbolAtDepth.displayDepth
        builder ++= s"${symbol.name}:$currentValue <= [[["
        builder ++= renderView(view, symbolAtDepth.displayDepth, adjustedLookBackDepth)
        builder ++= s"]]]($adjustedLookBackDepth)\n"
      }
    }

    val result = builder.toString()
    result
  }

  def render(symbol: Symbol, lookBackDepth: Int = 0): String = {
    symbolsToDo.enqueue(SymbolAtDepth(symbol, 0, lookBackDepth))

    renderInternal()
  }
}





