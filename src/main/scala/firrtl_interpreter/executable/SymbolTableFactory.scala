// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir._
import firrtl.{WDefInstance, WRef, WSubField, WSubIndex}
import firrtl_interpreter._
import firrtl_interpreter.utils.TSort
import logger._

import scala.collection.mutable

/**
  * A (probably overly complex) map of the names to expressions that occur in @circuit
  * This is used by the expression evaluator to follow dependencies
  * It also maintains lists or sets of ports, registers, memories, stop and printf statements.
  * The above information is created by the companion object which does the actual work
  * of traversing the circuit and discovering the various components and expressions
  *
  * @param circuit the AST being analyzed
  * @param module top level module in the AST, used elsewhere to find top level ports
  */
class SymbolTableFactory(
    val circuit: Circuit,
    val module: Module,
    val blackBoxFactories: Seq[BlackBoxFactory] = Seq.empty
  ) extends LazyLogging {

  type SymbolSet = Set[Symbol]

  val nameToSymbol = new mutable.HashMap[String, Symbol]()

  val dependencies: mutable.HashMap[Symbol, SymbolSet] = new mutable.HashMap[Symbol, SymbolSet]
  val registerNames: mutable.HashSet[String] = new mutable.HashSet[String]()

  def getInfo: String = {
    f"""
       |Circuit Info:
     """.stripMargin
  }

  // scalastyle:off
  def processDependencyStatements(modulePrefix: String, s: Statement): Unit = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def expressionToReferences(expression: Expression): SymbolSet = {
      val result = expression match {
        case Mux(condition, trueExpression, falseExpression, _) =>
          expressionToReferences(condition) ++
            expressionToReferences(trueExpression) ++
            expressionToReferences(falseExpression)

        case _: WRef | _: WSubField | _:WSubIndex =>
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

    s match {
      case block: Block =>
        block.stmts.foreach { subStatement =>
          processDependencyStatements(modulePrefix, subStatement)
        }

      case con: Connect =>
        con.loc match {
          case (_: WRef | _: WSubField | _: WSubIndex) =>
            val name = if(registerNames.contains(expand(con.loc.serialize))) {
              expand(con.loc.serialize) + "/in"
            }
            else {
              expand(con.loc.serialize)
            }
            val symbol = nameToSymbol(name)

            dependencies(symbol) = expressionToReferences(con.expr)
        }

      case WDefInstance(info, instanceName, moduleName, _) =>
        val subModule = FindModule(moduleName, circuit)
        val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
        logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
        processModule(newPrefix, subModule)

      case DefNode(info, name, expression) =>
        logger.debug(s"declaration:DefNode:$name:${expression.serialize} ${expressionToReferences(expression)}")
        val expandedName = expand(name)
        val symbol = Symbol(expandedName, expression.tpe)
        nameToSymbol(expandedName) = symbol
        dependencies(symbol) = expressionToReferences(expression)

      case DefWire(info, name, tpe) =>
        logger.debug(s"declaration:DefWire:$name")
        val expandedName = expand(name)
        val symbol = Symbol(expandedName, tpe)
        nameToSymbol(expandedName) = symbol
        dependencies(symbol) = Set.empty

      case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
        val expandedName = expand(name)

        val registerIn  = Symbol(expandedName + "/in", tpe)
        val registerOut = Symbol(expandedName, tpe)
        registerNames += registerOut.name
        nameToSymbol(registerIn.name) = registerIn
        nameToSymbol(registerOut.name) = registerOut
        dependencies(registerIn) = Set.empty
        dependencies(registerOut) = Set.empty

      case defMemory: DefMemory =>
        val expandedName = expand(defMemory.name)
        logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
        val newDefMemory = defMemory.copy(name = expandedName)

      //      case IsInvalid(info, expression) =>
      //        IsInvalid(info, expressionToReferences(expression))
      //      case Stop(info, ret, clkExpression, enableExpression) =>
      //        addStop(Stop(info, ret, expressionToReferences(clkExpression), expressionToReferences(enableExpression)))
      //        s
      //      case Print(info, stringLiteral, argExpressions, clkExpression, enableExpression) =>
      //        addPrint(Print(
      //          info, stringLiteral,
      //          argExpressions.map { expression => expressionToReferences(expression) },
      //          expressionToReferences(clkExpression),
      //          expressionToReferences(enableExpression)
      //        ))

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

    for(port <- extModule.ports) {
      if(port.direction == Output) {
        val outputDependencies = instance.outputDependencies(port.name)
        val expandedName = expand(port.name)
        val symbol = Symbol(expandedName, port.tpe)
        nameToSymbol(expandedName) = symbol
        dependencies(symbol) = Set.empty
      }
    }
  }

  def processModule(modulePrefix: String, myModule: DefModule): Unit = {
    def expand(name: String): String = if(modulePrefix.nonEmpty) modulePrefix + "." + name else name

    def processPorts(module: DefModule): Unit = {
      for(port <- module.ports) {
        val expandedName = expand(port.name)
        val symbol = Symbol(expandedName, port.tpe)
        nameToSymbol(expandedName) = symbol
        dependencies(symbol) = Set.empty
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
        if(! implementationFound) {
          println( s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
            """was not matched with an implementation""")
        }
    }
  }

  processModule("", module)

  logger.debug(s"For module ${module.name} dependencyGraph =")
  nameToSymbol.keys.toSeq.sorted foreach { k =>
    val symbol = nameToSymbol(k)
    println(f"$symbol%-30.30s ${dependencies(symbol).map(_.name).mkString(",").take(100)}")
  }

  TSort.showMissingTerminals(dependencies.toMap)
  println(s"Loops: ${TSort.findLoops(dependencies.toMap)}")

  val sorted: Iterable[Symbol] = TSort(dependencies.toMap, Seq.empty[Symbol])

  println(s"Sorted elements\n${sorted.map(_.name).mkString("\n")}")
  println(s"End of dependency graph")
  // scalastyle:on cyclomatic.complexity

}
