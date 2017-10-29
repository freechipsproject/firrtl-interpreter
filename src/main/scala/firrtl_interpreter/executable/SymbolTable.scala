// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl._
import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir._
import firrtl_interpreter.utils.TSort
import firrtl_interpreter.{BlackBoxFactory, BlackBoxImplementation, FindModule, InterpreterException}
import logger.LazyLogging

import scala.collection.immutable.Set
import scala.collection.mutable

class SymbolTable(nameToSymbol: mutable.HashMap[String, Symbol]) {
  private val sizeAndIndexToSymbol = new mutable.HashMap[DataSize, mutable.HashMap[Int, Symbol]] {
    override def default(key: DataSize): mutable.HashMap[Int, Symbol] = {
      this(key) = new mutable.HashMap[Int, Symbol]
      this(key)
    }
  }
  var keyDependsOnSymbols: DiGraph[Symbol] = new DiGraph[Symbol](Map.empty)
  var symbolDependsOnKeys: DiGraph[Symbol] = new DiGraph[Symbol](Map.empty)

  def allocateData(dataStore: DataStore): Unit = {
    nameToSymbol.values.foreach { symbol =>
      symbol.index = dataStore.getIndex(symbol.dataSize, symbol.slots)
      sizeAndIndexToSymbol(symbol.dataSize)(symbol.index) = symbol
    }
    dataStore.allocateBuffers()
  }

  def size: Int = nameToSymbol.size
  def keys:Iterable[String] = nameToSymbol.keys
  def symbols:Iterable[Symbol] = nameToSymbol.values

  def sortKey(dataSize: DataSize, index: Int): Int = sizeAndIndexToSymbol(dataSize)(index).cardinalNumber

  val registerNames:    mutable.HashSet[String] = new mutable.HashSet[String]
  val inputPortsNames:  mutable.HashSet[String] = new mutable.HashSet[String]
  val outputPortsNames: mutable.HashSet[String] = new mutable.HashSet[String]

  def isRegister(name: String): Boolean = registerNames.contains(name)

  def apply(name: String): Symbol = nameToSymbol(name)
  def apply(dataSize: DataSize, index: Int): Symbol = sizeAndIndexToSymbol(dataSize)(index)
  def apply(dataStore: DataStore, assigner: Assigner): Symbol = {
    val (size, index) = dataStore.getSizeAndIndex(assigner)
    apply(size, index)
  }

  def contains(name: String): Boolean = nameToSymbol.contains(name)

  def render: String = {
    Symbol.renderHeader + "\n" +
    keys.toArray.sorted.map { name =>
      nameToSymbol(name).render
    }.mkString("\n")
  }
}

object SymbolTable extends LazyLogging {
  def apply(nameToSymbol: mutable.HashMap[String, Symbol]): SymbolTable = new SymbolTable(nameToSymbol)

  //scalastyle:off cyclomatic.complexity method.length
  def apply(circuit: Circuit, blackBoxFactories: Seq[BlackBoxFactory] = Seq.empty): SymbolTable = {

    type SymbolSet = Set[Symbol]

    val nameToSymbol = new mutable.HashMap[String, Symbol]()

//    val dependencies: mutable.HashMap[Symbol, SymbolSet] = new mutable.HashMap[Symbol, SymbolSet]

    val keysDependOnSymbols: MutableDiGraph[Symbol] = new MutableDiGraph[Symbol]
    val symbolsDependOnKeys: MutableDiGraph[Symbol] = new MutableDiGraph[Symbol]

    val registerNames: mutable.HashSet[String] = new mutable.HashSet[String]()
    val inputPorts = new mutable.HashSet[String]
    val outputPorts = new mutable.HashSet[String]

    def getInfo: String = {
      f"""
         |Circuit Info:
     """.stripMargin
    }

    // scalastyle:off
    def processDependencyStatements(modulePrefix: String, s: Statement): Unit = {
      def expand(name: String): String = if (modulePrefix.isEmpty) name else modulePrefix + "." + name

      def expressionToReferences(expression: Expression): SymbolSet = {
        val result = expression match {
          case Mux(condition, trueExpression, falseExpression, _) =>
            expressionToReferences(condition) ++
              expressionToReferences(trueExpression) ++
              expressionToReferences(falseExpression)

          case _: WRef | _: WSubField | _: WSubIndex =>
            Set(nameToSymbol(expand(expression.serialize)))

          case ValidIf(condition, value, _) =>
            expressionToReferences(condition) ++ expressionToReferences(value)
          case DoPrim(_, args, _, _) =>
            args.foldLeft(Set.empty[Symbol]) { case (accum, expr) => accum ++ expressionToReferences(expr) }
          case _: UIntLiteral | _: SIntLiteral =>
            Set.empty[Symbol]
          case _ =>
            throw new Exception(s"expressionToReferences:error: unhandled expression $expression")
        }
        result
      }

      def addDependency(symbol: Symbol, dependentSymbols: Set[Symbol]): Unit = {
        dependentSymbols.foreach { dependentSymbol =>
          keysDependOnSymbols.addEdge(symbol, dependentSymbol)
          symbolsDependOnKeys.addEdge(dependentSymbol, symbol)
        }
      }

      s match {
        case block: Block =>
          block.stmts.foreach { subStatement =>
            processDependencyStatements(modulePrefix, subStatement)
          }

        case con: Connect =>
          con.loc match {
            case (_: WRef | _: WSubField | _: WSubIndex) =>
              val name = if (registerNames.contains(expand(con.loc.serialize))) {
                expand(con.loc.serialize) + "/in"
              }
              else {
                expand(con.loc.serialize)
              }
              val symbol = nameToSymbol(name)

              addDependency(symbol, expressionToReferences(con.expr))
          }

        case WDefInstance(_, instanceName, moduleName, _) =>
          val subModule = FindModule(moduleName, circuit)
          val newPrefix = if (modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
          logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
          processModule(newPrefix, subModule)

        case DefNode(info, name, expression) =>
          logger.debug(s"declaration:DefNode:$name:${expression.serialize} ${expressionToReferences(expression)}")
          val expandedName = expand(name)
          val symbol = Symbol(expandedName, expression.tpe, firrtl.NodeKind, info = info)
          nameToSymbol(expandedName) = symbol
          addDependency(symbol, expressionToReferences(expression))

        case DefWire(info, name, tpe) =>
          logger.debug(s"declaration:DefWire:$name")
          val expandedName = expand(name)
          val symbol = Symbol(expandedName, tpe, WireKind, info = info)
          nameToSymbol(expandedName) = symbol

        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
          val expandedName = expand(name)

          val registerIn = Symbol(expandedName + "/in", tpe, RegKind, info = info)
          val registerOut = Symbol(expandedName, tpe,RegKind, info = info)
          registerNames += registerOut.name
          nameToSymbol(registerIn.name) = registerIn
          nameToSymbol(registerOut.name) = registerOut

        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")

          Memory.buildSymbols(defMemory, expandedName, keysDependOnSymbols, symbolsDependOnKeys).foreach { symbol =>
            nameToSymbol(symbol.name) = symbol
          }


        //      case IsInvalid(info, expression) =>
        //        IsInvalid(info, expressionToReferences(expression))
        case _: Stop   =>
        case _: Print  =>
        case EmptyStmt =>

        case conditionally: Conditionally =>
          // logger.debug(s"got a conditionally $conditionally")
          throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
        case _ =>
          println(s"TODO: Unhandled statement $s")
      }
    }

    // scalastyle:on

    def processExternalInstance(extModule: ExtModule,
                                modulePrefix: String,
                                instance: BlackBoxImplementation): Unit = {
      def expand(name: String): String = modulePrefix + "." + name

      for (port <- extModule.ports) {
        if (port.direction == Output) {
          val outputDependencies = instance.outputDependencies(port.name)
          val expandedName = expand(port.name)
          val symbol = Symbol(expandedName, port.tpe, PortKind)
          nameToSymbol(expandedName) = symbol
        }
      }
    }

    def processModule(modulePrefix: String, myModule: DefModule): Unit = {
      def expand(name: String): String = if (modulePrefix.nonEmpty) modulePrefix + "." + name else name

      def processPorts(module: DefModule): Unit = {
        for (port <- module.ports) {
          val expandedName = expand(port.name)
          val symbol = Symbol(expandedName, port.tpe, PortKind)
          nameToSymbol(expandedName) = symbol
          if(modulePrefix.isEmpty) {  // this is true only at top level
            if(port.direction == Input) {
              inputPorts += symbol.name
            }
            else if(port.direction == Output) {
              outputPorts += symbol.name
            }
          }
        }
      }

      myModule match {
        case module: Module =>
          processPorts(module)
          processDependencyStatements(modulePrefix, module.body)
        case extModule: ExtModule => // Look to see if we have an implementation for this
          logger.debug(s"got external module ${extModule.name} instance $modulePrefix")
          processPorts(extModule)
          /* use exists while looking for the right factory, short circuits iteration when found */
          logger.debug(s"Factories: ${blackBoxFactories.mkString("\n")}")
          val implementationFound = blackBoxFactories.exists { factory =>
            logger.debug("Found an existing factory")
            factory.createInstance(modulePrefix, extModule.defname) match {
              case Some(implementation) =>
                processExternalInstance(extModule, modulePrefix, implementation)
                true
              case _ => false
            }
          }
          if (!implementationFound) {
            println(
              s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
                """was not matched with an implementation""")
          }
      }
    }

    val module = FindModule(circuit.main, circuit) match {
      case regularModule: firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw InterpreterException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw InterpreterException(s"Top level module is not the right kind of module $x")
    }

    processModule("", module)

    val keysDependOnSymbolsDiGraph = DiGraph(keysDependOnSymbols)
    val symbolsDependOnKeysDiGraph = DiGraph(symbolsDependOnKeys)


    val sorted: Seq[Symbol] = symbolsDependOnKeysDiGraph.linearize

    sorted.zipWithIndex.foreach { case (symbol, index) => symbol.cardinalNumber = index }

    logger.debug(s"Sorted elements\n${sorted.map(_.name).mkString("\n")}")
    logger.info(s"End of dependency graph")
    // scalastyle:on cyclomatic.complexity

    val symbolTable = SymbolTable(nameToSymbol)
    symbolTable.registerNames ++= registerNames
    symbolTable.inputPortsNames    ++= inputPorts
    symbolTable.outputPortsNames   ++= outputPorts

    symbolTable
  }
}
