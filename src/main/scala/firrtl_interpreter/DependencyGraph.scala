// See LICENSE for license details.

package firrtl_interpreter

import firrtl._
import firrtl.ir._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * contains the constructor for a dependency graph.  The code for traversing a circuit
  * and discovering the components and the expressions lives here
  */
object DependencyGraph extends SimpleLogger {
  val MaxColumnWidth = 100 // keeps displays of expressions readable

  /**
    * finds the specified module name in the circuit
    *
    * @param moduleName name to find
    * @param circuit circuit being analyzed
    * @return the circuit, exception occurs in not found
    */
  def findModule(moduleName: String, circuit: Circuit): DefModule = {
    circuit.modules.find(module => module.name == moduleName) match {
      case Some(module: Module) =>
        module
      case Some(externalModule: DefModule) =>
        externalModule
      case _ =>
        throw InterpreterException(s"Could not find top level module in $moduleName")
    }
  }

  // scalastyle:off
  def processDependencyStatements(modulePrefix: String, s: Statement, dependencyGraph: DependencyGraph): Statement = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def renameExpression(expression: Expression): Expression = {
      dependencyGraph.numberOfNodes += 1
      val result = expression match {
        case Mux(condition, trueExpression, falseExpression, tpe) =>
          dependencyGraph.numberOfMuxes += 1
          Mux(
            renameExpression(condition),
            renameExpression(trueExpression),
            renameExpression(falseExpression),
            tpe
          )
        case WRef(name, tpe, kind, gender) => WRef(expand(name), tpe, kind, gender)
        case WSubField(subExpression, name, tpe, gender) =>
          WSubField(renameExpression(subExpression), name, tpe, gender)
        case WSubIndex(subExpression, value, tpe, gender) =>
          WSubIndex(renameExpression(subExpression), value, tpe, gender)
        case ValidIf(condition, value, tpe) => ValidIf(renameExpression(condition), renameExpression(value), tpe)
        case DoPrim(op, args, const, tpe) =>
          DoPrim(op, args.map { subExpression => renameExpression(subExpression)}, const, tpe)
        case c: UIntLiteral => c
        case c: SIntLiteral => c
        case _ =>
          throw new Exception(s"renameExpression:error: unhandled expression $expression")
      }
      result
    }

    dependencyGraph.numberOfStatements += 1
    s match {
      case block: Block =>
        block.stmts.map { subStatement =>
          processDependencyStatements(modulePrefix, subStatement, dependencyGraph)
        }
        block
      case con: Connect =>
        con.loc match {
          case WRef(name, _, _, _) => dependencyGraph(expand(name)) = renameExpression(con.expr)
          case (_: WSubField | _: WSubIndex) => dependencyGraph(expand(con.loc.serialize)) = renameExpression(con.expr)
        }
        con
      case WDefInstance(info, instanceName, moduleName, _) =>
        val subModule = findModule(moduleName, dependencyGraph.circuit)
        val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
        log(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
        processModule(newPrefix, subModule, dependencyGraph)

        subModule match {
          case _: Module =>
          case extModule: ExtModule => // Look to see if we have an implementation for this
            log(s"got external module ${extModule.name} instance $modulePrefix")
            /* use exists while looking for the right factory, short circuits iteration when found */
            log(s"Factories: ${dependencyGraph.blackBoxFactories.mkString("\n")}")
            val implementationFound = dependencyGraph.blackBoxFactories.exists { factory =>
              log("Found an existing factory")
              factory.createInstance(modulePrefix, extModule.defname) match {
                case Some(implementation) =>
                  processExternalInstance(extModule, newPrefix, instanceName, implementation, dependencyGraph)
                  true
                case _ => false
              }
            }
            if(! implementationFound) {
              println( s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
                """was not matched with an implementation""")
            }
        }

        dependencyGraph.addSourceInfo(newPrefix, info)
        dependencyGraph.addInstanceName(instanceName, moduleName)
        s
      case DefNode(info, name, expression) =>
        log(s"declaration:DefNode:$name:${expression.serialize} ${renameExpression(expression).serialize}")
        val expandedName = expand(name)
        dependencyGraph.nodes += expandedName
        dependencyGraph.recordName(expandedName)
        dependencyGraph(expandedName) = renameExpression(expression)
        dependencyGraph.addSourceInfo(expandedName, info)
        s
      case DefWire(info, name, tpe) =>
        log(s"declaration:DefWire:$name")
        val expandedName = expand(name)
        dependencyGraph.wires += expandedName
        dependencyGraph.recordName(expandedName)
        dependencyGraph.recordType(expandedName, tpe)
        dependencyGraph.addSourceInfo(expandedName, info)
        s
      case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
        log(s"declaration:DefRegister:$name clock <- ${clockExpression.serialize} ${renameExpression(clockExpression).serialize}")
        log(s"declaration:DefRegister:$name reset <- ${resetExpression.serialize} ${renameExpression(resetExpression).serialize}")
        log(s"declaration:DefRegister:$name init  <- ${initValueExpression.serialize} ${renameExpression(initValueExpression).serialize}")
        val renamedDefRegister = DefRegister(
          info, expand(name), tpe,
          renameExpression(clockExpression),
          renameExpression(resetExpression),
          renameExpression(initValueExpression)
        )
        val expandedName = expand(name)
        dependencyGraph.registerNames += expandedName
        dependencyGraph.recordName(expandedName)
        dependencyGraph.recordType(expandedName, tpe)
        dependencyGraph.registers(expandedName) = renamedDefRegister
        dependencyGraph.addSourceInfo(expandedName, info)
        s
      case defMemory: DefMemory =>
        val expandedName = expand(defMemory.name)
        log(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
        val newDefMemory = defMemory.copy(name = expandedName)
        dependencyGraph.addMemory(newDefMemory)
        dependencyGraph.addSourceInfo(expandedName, defMemory.info)
        s
      case IsInvalid(info, expression) =>
        IsInvalid(info, renameExpression(expression))
      case Stop(info, ret, clkExpression, enableExpression) =>
        dependencyGraph.addStop(Stop(info, ret, renameExpression(clkExpression), renameExpression(enableExpression)))
        s
      case Print(info, stringLiteral, argExpressions, clkExpression, enableExpression) =>
        dependencyGraph.addPrint(Print(
          info, stringLiteral,
          argExpressions.map { expression => renameExpression(expression) },
          renameExpression(clkExpression),
          renameExpression(enableExpression)
        ))
        s
      case EmptyStmt =>
        s
      case conditionally: Conditionally =>
        // log(s"got a conditionally $conditionally")
        throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
      case _ =>
        println(s"TODO: Unhandled statement $s")
        s
    }
  }
  // scalastyle:on

  def processExternalInstance(extModule: ExtModule,
                              modulePrefix: String,
                              instanceName: String,
                              instance: BlackBoxImplementation,
                              dependencyGraph: DependencyGraph): Unit = {
    def expand(name: String): String = modulePrefix + "." + name

    for(port <- extModule.ports) {
      if(port.direction == Output) {
        val outputDependencies = instance.outputDependencies(port.name)
        val dependendInputs = outputDependencies.map(s => s"$modulePrefix.$s")
        dependencyGraph(expand(port.name)) = BlackBoxOutput(port.name, instance, dependendInputs, port.tpe)
      }
    }
  }

  def processModule(modulePrefix: String, myModule: DefModule, dependencyGraph: DependencyGraph): Unit = {
    def processPorts(module: DefModule): Unit = {
      for(port <- module.ports) {
        if(modulePrefix.isEmpty) {
          /* We are processing a  module at the TOP level, which is indicated by it's lack of prefix */
          dependencyGraph.nameToType(port.name) = port.tpe
          if (port.direction == Input) {
            dependencyGraph.inputPorts += port.name
            dependencyGraph.recordName(port.name)
          }
          else if (port.direction == Output) {
            dependencyGraph.outputPorts += port.name
            dependencyGraph.recordName(port.name)
          }
        }
        else {
          /* We are processing a sub-module */
          dependencyGraph.nameToType(modulePrefix + "." + port.name) = port.tpe
          dependencyGraph.recordName(modulePrefix + "." + port.name)
          dependencyGraph.inlinedPorts += modulePrefix + "." + port.name
        }
      }
    }
    myModule match {
      case module: Module =>
        processPorts(module)
        processDependencyStatements(modulePrefix, module.body, dependencyGraph)
      case extModule: ExtModule => // Look to see if we have an implementation for this
        log(s"got external module ${extModule.name} instance $modulePrefix")
        processPorts(extModule)
    }
  }

  // scalastyle:off cyclomatic.complexity
  def apply(circuit: Circuit, interpreter: FirrtlTerp): DependencyGraph = {
    val module = findModule(circuit.main, circuit) match {
      case regularModule: Module => regularModule
      case externalModule: ExtModule =>
        throw InterpreterException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw InterpreterException(s"Top level module is not the right kind of module $x")
    }

    val dependencyGraph = new DependencyGraph(circuit, module, interpreter.blackBoxFactories)

    dependencyGraph.numberOfNodes = 0
    dependencyGraph.numberOfStatements = 0

    //    setVerbose(true)

    processModule("", module, dependencyGraph)

    for(name <- dependencyGraph.validNames) {
      if(! dependencyGraph.nameToExpression.contains(name)) {
        val defaultValue = dependencyGraph.nameToType(name) match {
          case UIntType(width) => UIntLiteral(0, width)
          case SIntType(width) => SIntLiteral(0, width)
          case ClockType       => UIntLiteral(0, IntWidth(1))
          case _ =>
            throw new Exception(s"error can't find default value for $name.type = ${dependencyGraph.nameToType(name)}")
        }
        dependencyGraph.nameToExpression(name) = defaultValue
      }
    }

    log(s"For module ${module.name} dependencyGraph =")
    dependencyGraph.nameToExpression.keys.toSeq.sorted foreach { k =>
      val v = dependencyGraph.nameToExpression(k).serialize
      log(s"  $k -> (" + v.toString.take(MaxColumnWidth) + ")")
    }
    println(s"End of dependency graph")
    dependencyGraph
  }
  // scalastyle:on cyclomatic.complexity
}

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
class DependencyGraph(val circuit: Circuit,
                      val module: Module,
                      val blackBoxFactories: Seq[BlackBoxFactory] = Seq.empty) {
  val nameToExpression = new scala.collection.mutable.HashMap[String, Expression]
  val validNames       = new mutable.HashSet[String]
  val nameToType       = new mutable.HashMap[String, Type]
  val registerNames    = new mutable.HashSet[String]
  val registers        = new mutable.HashMap[String, DefRegister]
  val memories         = new mutable.HashMap[String, Memory]
  val memoryKeys       = new mutable.HashMap[String, Memory]
  val memoryOutputKeys = new mutable.HashMap[String, Seq[String]]
  val stops            = new ArrayBuffer[Stop]
  val prints           = new ArrayBuffer[Print]
  val sourceInfo       = new mutable.HashMap[String, String]
  val instanceNames    = new mutable.HashMap[String, String]

  val inputPorts       = new mutable.HashSet[String]
  val outputPorts      = new mutable.HashSet[String]
  val nodes            = new mutable.HashSet[String]
  val wires            = new mutable.HashSet[String]
  val inlinedPorts     = new mutable.HashSet[String]

  var numberOfStatements = 0
  var numberOfNodes = 0
  var numberOfMuxes = 0

  def update(key: String, e: Expression): Unit = nameToExpression(key) = e
  def apply(key: String): Option[Expression] = {
    recordName(key)
    nameToExpression.get(key)
  }
  def keys: Iterable[String] = nameToExpression.keys
  def recordName(key: String): Unit = validNames += key
  def recordType(key: String, tpe: Type): Unit = {nameToType(key) = tpe}
  def getType(key: String): Type = nameToType(key)
  def addStop(stopStatement: Stop): Unit = { stops += stopStatement }
  def addPrint(printStatement: Print): Unit = { prints += printStatement }

  def addMemory(defMemory: DefMemory): Memory = {
    val newMemory = Memory(defMemory)
    memories(defMemory.name) = newMemory
    for((portKey, dependentPorts) <- newMemory.getAllOutputFields) {
      memoryKeys(portKey) = newMemory
      memoryOutputKeys(portKey) = dependentPorts
    }
    newMemory
  }

  def getInfo: String = {
    f"""
       |Circuit Info:
       |  Statements:      $numberOfStatements%11d
       |  Nodes:           $numberOfNodes%11d
       |  Muxes:           $numberOfMuxes%11d
       |  Expressions:     ${nameToExpression.size}%11d
     """.stripMargin
  }

  def addSourceInfo(name: String, info: Info): Unit = {
    info match {
      case f: FileInfo => sourceInfo(name) = f.toString
      case _ => // I don't know what to do with other annotations
    }
  }

  def addInstanceName(instanceName: String, moduleName: String): Unit = {
    instanceNames(instanceName) = moduleName
  }

  def hasInput(name: String): Boolean = inputPorts.contains(name)
  def hasOutput(name: String): Boolean = outputPorts.contains(name)

  def getInfo(name: String): String = {
    sourceInfo.getOrElse(name, "")
  }

  def addKind(key: String): String = {
    if(registerNames.contains(key)) {
      key + ":Reg"
    }
    else if(memoryKeys.contains(key)) {
      key + ":MemIO"
    }
    else if(inputPorts.contains(key)) {
      key + ":Input"
    }
    else if(outputPorts.contains(key)) {
      key + ":Output"
    }
    else if(nodes.contains(key)) {
      key + ":Node"
    }
    else if(wires.contains(key)) {
      key + ":Wire"
    }
    else if(inlinedPorts.contains(key)) {
      key + ":InlinedPort"
    }
    else {
      key
    }
  }
}
