// See LICENSE for license details.

package firrtl_interpreter

import firrtl._

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
  def findModule(moduleName: String, circuit: Circuit): Module = {
    circuit.modules.find(module => module.name == moduleName) match {
      case Some(module) =>
        module
      case _ =>
        throw InterpreterException(s"Could not find top level module in $moduleName")
    }
  }

  // scalastyle:off
  def processDependencyStatements(modulePrefix: String, s: Stmt, dependencyGraph: DependencyGraph): Stmt = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def renameExpression(expression: Expression): Expression = {
      dependencyGraph.numberOfNodes += 1
      val result = expression match {
        case Mux(condition, trueExpression, falseExpression, tpe) =>
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
          DoPrim(op, args.map {case subExpression => renameExpression(subExpression)}, const, tpe)
        case c: UIntValue => c
        case c: SIntValue => c
        case _ =>
          throw new Exception(s"renameExpression:error: unhandled expression $expression")
      }
      result
    }

    dependencyGraph.numberOfStatements += 1
    s match {
      case begin: Begin =>
        begin.stmts.map { case subStatement =>
          processDependencyStatements(modulePrefix, subStatement, dependencyGraph)
        }
        begin
      case con: Connect =>
        con.loc match {
          case WRef(name, _, _, _) => dependencyGraph(expand(name)) = renameExpression(con.exp)
          case (_: WSubField | _: WSubIndex) => dependencyGraph(expand(con.loc.serialize)) = renameExpression(con.exp)
        }
        con
      case WDefInstance(info, instanceName, moduleName, tpe) =>
        val subModule = findModule(moduleName, dependencyGraph.circuit)
        val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
        log(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
        processModule(newPrefix, subModule, dependencyGraph)
        dependencyGraph.addSourceInfo(newPrefix, info)
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
        dependencyGraph.registers += renamedDefRegister
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
          argExpressions.map { case expression => renameExpression(expression) },
          renameExpression(clkExpression),
          renameExpression(enableExpression)
        ))
        s
      case e: Empty =>
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

  def processModule(modulePrefix: String, module: Module, dependencyGraph: DependencyGraph): Unit = {
    module match {
      case i: InModule =>
        for(port <- i.ports) {
          if(modulePrefix.isEmpty) {
            dependencyGraph.nameToType(port.name) = port.tpe
            if (port.direction == INPUT) {
              dependencyGraph.inputPorts += port.name
              dependencyGraph.recordName(port.name)
            }
            else if (port.direction == OUTPUT) {
              dependencyGraph.outputPorts += port.name
              dependencyGraph.recordName(port.name)
            }
          }
          else {
            dependencyGraph.nameToType(modulePrefix + "." + port.name) = port.tpe
            dependencyGraph.recordName(modulePrefix + "." + port.name)
            dependencyGraph.inlinedPorts += modulePrefix + "." + port.name
          }
        }
        processDependencyStatements(modulePrefix, i.body, dependencyGraph)
      case e: ExModule => // Do nothing
    }
  }

  def apply(circuit: Circuit): DependencyGraph = {
    val module = findModule(circuit.main, circuit)
    val dependencyGraph = new DependencyGraph(circuit, module)

    dependencyGraph.numberOfNodes = 0
    dependencyGraph.numberOfStatements = 0

    //    setVerbose(true)

    processModule("", module, dependencyGraph)

    for(name <- dependencyGraph.validNames) {
      if(! dependencyGraph.nameToExpression.contains(name)) {
        val defaultValue = dependencyGraph.nameToType(name) match {
          case UIntType(width) => UIntValue(0, width)
          case SIntType(width) => SIntValue(0, width)
          case ClockType()     => UIntValue(0, IntWidth(1))
          case _ =>
            throw new Exception(s"error can't find default value for $name.type = ${dependencyGraph.nameToType(name)}")
        }
        dependencyGraph.nameToExpression(name) = defaultValue
      }
    }

    log(s"For module ${module.name} dependencyGraph =")
    dependencyGraph.nameToExpression.keys.toSeq.sorted foreach { case k =>
      val v = dependencyGraph.nameToExpression(k).serialize
      log(s"  $k -> (" + v.toString.take(MaxColumnWidth) + ")")
    }
    println(s"End of dependency graph")
    dependencyGraph
  }
}

/**
  * A (probably overly complex) map of the names to expressions that occur in @circuit
  * This is used by the expression evaluator to follow dependencys
  * It also maintains lists or sets of ports, registers, memories, stop and printf statements.
  * The above information is created by the companion object which does the actual work
  * of traversing the circuit and discovering the various components and expressions
  *
  * @param circuit the AST being analyzed
  * @param module top level module in the AST, used elsewhere to find top level ports
  */
class DependencyGraph(val circuit: Circuit, val module: Module) {
  val nameToExpression = new scala.collection.mutable.HashMap[String, Expression]
  val validNames       = new mutable.HashSet[String]
  val nameToType       = new mutable.HashMap[String, Type]
  val registerNames    = new mutable.HashSet[String]
  val registers        = new ArrayBuffer[DefRegister]
  val memories         = new mutable.HashMap[String, Memory]
  val memoryKeys       = new mutable.HashMap[String, Memory]
  val memoryOutputKeys = new mutable.HashMap[String, Seq[String]]
  val stops            = new ArrayBuffer[Stop]
  val prints           = new ArrayBuffer[Print]
  val sourceInfo       = new mutable.HashMap[String, String]

  val inputPorts       = new mutable.HashSet[String]
  val outputPorts      = new mutable.HashSet[String]
  val nodes            = new mutable.HashSet[String]
  val wires            = new mutable.HashSet[String]
  val inlinedPorts     = new mutable.HashSet[String]

  var numberOfStatements = 0
  var numberOfNodes = 0

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

  def addSourceInfo(name: String, info: Info): Unit = {
    info match {
      case f: FileInfo => sourceInfo(name) = f.toString
      case _ => // I don't know what to do with other annotations
    }
  }

  def hasInput(name: String): Boolean = inputPorts.contains(name)
  def hasOutput(name: String): Boolean = outputPorts.contains(name)

  def getInfo(name: String): String = {
    sourceInfo.getOrElse(name, "").toString
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
