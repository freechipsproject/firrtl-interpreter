// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl._
import firrtl.graph.{DiGraph, MutableDiGraph}
import firrtl.ir._
import firrtl_interpreter.{BlackBoxFactory, BlackBoxImplementation, FindModule, InterpreterException}
import logger.LazyLogging

import scala.collection.immutable.Set
import scala.collection.mutable

class SymbolTable(nameToSymbol: mutable.HashMap[String, Symbol]) {

  var childrenOf: DiGraph[Symbol] = DiGraph[Symbol](Map.empty[Symbol, Set[Symbol]])
  var parentsOf: DiGraph[Symbol] = DiGraph[Symbol](Map.empty[Symbol, Set[Symbol]])

  var orphans: Seq[Symbol] = Seq.empty

  private val toAssigner: mutable.HashMap[Symbol, Assigner] = new mutable.HashMap()
  def addAssigner(symbol: Symbol, assigner: Assigner): Unit = {
    if(toAssigner.contains(symbol)) {
      throw new InterpreterException(s"Assigner already exists for $symbol")
    }
    toAssigner(symbol) = assigner
  }

  def allocateData(dataStore: DataStore): Unit = {
    nameToSymbol.values.foreach { symbol =>
      symbol.index = dataStore.getIndex(symbol.dataSize, symbol.slots)
    }
    dataStore.allocateBuffers()
  }

  def size: Int = nameToSymbol.size
  def keys:Iterable[String] = nameToSymbol.keys
  def symbols:Iterable[Symbol] = nameToSymbol.values

  val registerNames:    mutable.HashSet[String] = new mutable.HashSet[String]
  val inputPortsNames:  mutable.HashSet[String] = new mutable.HashSet[String]
  val outputPortsNames: mutable.HashSet[String] = new mutable.HashSet[String]

  def isRegister(name: String): Boolean = registerNames.contains(name)
  def isTopLevelInput(name: String): Boolean = inputPortsNames.contains(name)

  def apply(name: String): Symbol = nameToSymbol(name)

  def getSymbolFromGetter(expressionResult: ExpressionResult, dataStore: DataStore): Option[Symbol] = {
    expressionResult match {
      case dataStore.GetInt(index)  => symbols.find { symbol => symbol.dataSize == IntSize && symbol.index == index}
      case dataStore.GetLong(index) => symbols.find { symbol => symbol.dataSize == LongSize && symbol.index == index}
      case dataStore.GetBig(index)  => symbols.find { symbol => symbol.dataSize == BigSize && symbol.index == index}
      case _ => None
    }
  }

  def getParents(symbols: Seq[Symbol]): Set[Symbol] = {
    symbols.flatMap { symbol =>
      parentsOf.reachableFrom(symbol)
    }.toSet
  }

  def getChildren(symbols: Seq[Symbol]): Set[Symbol] = {
    symbols.flatMap { symbol =>
      childrenOf.reachableFrom(symbol)
    }.toSet
  }

  def inputChildrenAssigners(): Seq[Assigner] = {
    val assigners = getChildren(inputPortsNames.map(nameToSymbol(_)).toSeq)
      .flatMap { symbol => toAssigner.get(symbol)}
      .toSeq
    assigners
  }

  def getAssigners(symbols: Seq[Symbol]): Seq[Assigner] = {
    val assigners = symbols.flatMap { symbol => toAssigner.get(symbol) }
    assigners
  }

  def get(name: String): Option[Symbol] = nameToSymbol.get(name)
  def getOrElse(name: String, default: => Symbol): Symbol = nameToSymbol.getOrElse(name, default)

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

    val sensitivityGraphBuilder: SensitivityGraphBuilder = new SensitivityGraphBuilder

    val registerNames: mutable.HashSet[String] = new mutable.HashSet[String]()
    val inputPorts = new mutable.HashSet[String]
    val outputPorts = new mutable.HashSet[String]

    def recordDependency(symbolA: Symbol, symbolB: Symbol): Unit = {
      sensitivityGraphBuilder.addSensitivity(symbolB, symbolA)
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
          recordDependency(symbol, dependentSymbol)
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

        case DefRegister(info, name, tpe, clockExpression, resetExpression, _) =>
          val expandedName = expand(name)

          val registerIn = Symbol(expandedName + "/in", tpe, RegKind, info = info)
          val registerOut = Symbol(expandedName, tpe,RegKind, info = info)
          registerNames += registerOut.name
          nameToSymbol(registerIn.name) = registerIn
          nameToSymbol(registerOut.name) = registerOut

          addDependency(registerOut, expressionToReferences(clockExpression))
          addDependency(registerIn, expressionToReferences(resetExpression))

        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")

          Memory.buildSymbols(defMemory, expandedName, sensitivityGraphBuilder).foreach { symbol =>
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
          instance.outputDependencies(port.name).foreach { inputPortName =>
            recordDependency(nameToSymbol(expand(port.name)), nameToSymbol(expand(inputPortName)))
          }
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

    // scalastyle:on cyclomatic.complexity

    val symbolTable = SymbolTable(nameToSymbol)
    symbolTable.registerNames    ++= registerNames
    symbolTable.inputPortsNames  ++= inputPorts
    symbolTable.outputPortsNames ++= outputPorts
    symbolTable.parentsOf        = sensitivityGraphBuilder.getParentsOfDiGraph
    symbolTable.childrenOf       = sensitivityGraphBuilder.getChildrenOfDiGraph

    val sorted: Seq[Symbol] = symbolTable.childrenOf.linearize

    sorted.zipWithIndex.foreach { case (symbol, index) => symbol.cardinalNumber = index }

    logger.debug(s"Sorted elements\n${sorted.map(_.name).mkString("\n")}")
    logger.info(s"End of dependency graph")

    symbolTable.orphans = sensitivityGraphBuilder.orphans(symbolTable)

    symbolTable
  }
}
