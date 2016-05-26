// See LICENSE for license details.

package firrtl_interpreter

import firrtl._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object DependencyGraph extends SimpleLogger {
  val MaxColumnWidth = 100 // keeps displays of expressions readable
  var statements = 0
  var nodes = 0

  def findTopLevelModule(moduleName: String, circuit: Circuit): Module = {
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
      nodes += 1
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

    def showNode(kind: String, name: String, expression: String, renamedExpression: String = ""): Unit = {
      log(s"declaration:$kind:$name $expression $renamedExpression")
    }

    statements += 1
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
        val subModule = findTopLevelModule(moduleName, dependencyGraph.circuit)
        val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
        showNode("WDefInstance", instanceName, moduleName, s"prefix now $newPrefix")
        processModule(newPrefix, subModule, dependencyGraph)
        s
      case DefNode(_, name, expression) =>
        showNode("DefNode", name, expression.serialize, renameExpression(expression).serialize)
        dependencyGraph.nodes += expand(name)
        dependencyGraph.recordName(expand(name))
        dependencyGraph(expand(name)) = renameExpression(expression)
        s
      case DefWire(info, name, tpe) =>
        showNode("DefWire", name, "")
        dependencyGraph.wires += expand(name)
        dependencyGraph.recordName(expand(name))
        dependencyGraph.recordType(expand(name), tpe)
        s
      case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
        showNode("DefRegister", name, s"clock <- ${clockExpression.serialize}", renameExpression(clockExpression).serialize)
        showNode("DefRegister", name, s"clock <- ${resetExpression.serialize}", renameExpression(resetExpression).serialize)
        showNode("DefRegister", name, s"clock <- ${initValueExpression.serialize}", renameExpression(initValueExpression).serialize)
        val renamedDefRegister = DefRegister(
          info, name, tpe,
          renameExpression(clockExpression),
          renameExpression(resetExpression),
          renameExpression(initValueExpression)
        )
        dependencyGraph.registerNames += expand(name)
        dependencyGraph.recordName(expand(name))
        dependencyGraph.recordType(expand(name), tpe)
        dependencyGraph.registers += renamedDefRegister
        s
      case defMemory: DefMemory =>
        showNode("DefMemory", defMemory.name, "")
        //TODO: the name of the memory needs to be expanded, if memories are still present in firrtl
        val newDefMemory = defMemory.copy(name = expand(defMemory.name))
        dependencyGraph.addMemory(newDefMemory)
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
    val module = findTopLevelModule(circuit.main, circuit)
    nodes = 0
    statements = 0

    val dependencyGraph = new DependencyGraph(circuit, module)

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

  val inputPorts       = new mutable.HashSet[String]
  val outputPorts      = new mutable.HashSet[String]
  val nodes            = new mutable.HashSet[String]
  val wires            = new mutable.HashSet[String]
  val inlinedPorts     = new mutable.HashSet[String]

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

  def hasInput(name: String): Boolean = inputPorts.contains(name)
  def hasOutput(name: String): Boolean = outputPorts.contains(name)

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
