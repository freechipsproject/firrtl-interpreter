// See LICENSE for license details.

package firrtl_interpreter

import firrtl._
import firrtl.ir._
import logger.LazyLogging

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import logger._

/**
  * contains the constructor for a dependency graph.  The code for traversing a circuit
  * and discovering the components and the expressions lives here
  */
object DependencyTracker extends LazyLogging {
  val MaxColumnWidth = 100 // keeps displays of expressions readable

  type DependencySet = Set[String]
}

import DependencyTracker._

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
class DependencyTracker(val circuit: Circuit,
                      val module: Module,
                      val blackBoxFactories: Seq[BlackBoxFactory] = Seq.empty) {

  val MaxColumnWidth = 100 // keeps displays of expressions readable

  type DependencySet = Set[String]

  val dependencies: mutable.HashMap[String, DependencySet] = new mutable.HashMap[String, DependencySet]

  val registerDependencies: mutable.HashMap[String, DependencySet] = new mutable.HashMap[String, DependencySet]

  val resetDependencies: mutable.HashMap[String, DependencySet] = new mutable.HashMap[String, DependencySet]

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


  // scalastyle:off
  def processDependencyStatements(modulePrefix: String, s: Statement): Unit = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def expressionToReferences(expression: Expression): DependencySet = {
      numberOfNodes += 1
      val result = expression match {
        case Mux(condition, trueExpression, falseExpression, tpe) =>
          numberOfMuxes += 1
          expressionToReferences(condition) ++
            expressionToReferences(trueExpression) ++
            expressionToReferences(falseExpression)

        case WRef(name, tpe, kind, gender) =>
          Set(expand(name))
        case WSubField(subExpression, name, tpe, gender) =>
          Set(expand(expression.serialize))
        case WSubIndex(subExpression, value, tpe, gender) =>
          Set(expand(expression.serialize))

        case ValidIf(condition, value, tpe) =>
          expressionToReferences(condition) ++ expressionToReferences(value)
        case DoPrim(op, args, const, tpe) =>
          args.foldLeft(Set.empty[String]) { case (accum, expr) => accum ++ expressionToReferences(expr) }
        case c: UIntLiteral =>
          Set.empty[String]
        case c: SIntLiteral =>
          Set.empty[String]
        case _ =>
          throw new Exception(s"expressionToReferences:error: unhandled expression $expression")
      }
      result
    }

    numberOfStatements += 1
    s match {
      case block: Block =>
        block.stmts.foreach { subStatement =>
          processDependencyStatements(modulePrefix, subStatement)
        }

      case con: Connect =>
        con.loc match {
          case (_: WRef | _: WSubField | _: WSubIndex) =>
            val name = if(registerDependencies.contains(expand(con.loc.serialize))) {
              expand(con.loc.serialize) + "/in"
            }
            else {
              expand(con.loc.serialize)
            }

            dependencies(name) = expressionToReferences(con.expr)
        }

      case WDefInstance(info, instanceName, moduleName, _) =>
        val subModule = FindModule(moduleName, circuit)
        val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
        logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
        processModule(newPrefix, subModule)
        addSourceInfo(newPrefix, info)
        addInstanceName(instanceName, moduleName)

      case DefNode(info, name, expression) =>
        logger.debug(s"declaration:DefNode:$name:${expression.serialize} ${expressionToReferences(expression)}")
        val expandedName = expand(name)
        nodes += expandedName
        recordName(expandedName)
        addSourceInfo(expandedName, info)
        dependencies(expand(name)) = expressionToReferences(expression)

      case DefWire(info, name, tpe) =>
        logger.debug(s"declaration:DefWire:$name")
        val expandedName = expand(name)
        wires += expandedName
        recordName(expandedName)
        recordType(expandedName, tpe)
        addSourceInfo(expandedName, info)

      case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
        val expandedName = expand(name)

        registerDependencies(expandedName) = Set(s"$expandedName/in")
        resetDependencies(expandedName)    = expressionToReferences(initValueExpression)


        registerNames += expandedName
        recordName(expandedName)
        recordType(expandedName, tpe)
        addSourceInfo(expandedName, info)

      case defMemory: DefMemory =>
        val expandedName = expand(defMemory.name)
        logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
        val newDefMemory = defMemory.copy(name = expandedName)
        addMemory(newDefMemory)
        addSourceInfo(expandedName, defMemory.info)

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
        dependencies(expand(port.name)) = Set(port.name)
      }
    }
  }

  def processModule(modulePrefix: String, myModule: DefModule): Unit = {
    def processPorts(module: DefModule): Unit = {
      for(port <- module.ports) {
        if(modulePrefix.isEmpty) {
          /* We are processing a  module at the TOP level, which is indicated by it's lack of prefix */
          nameToType(port.name) = port.tpe
          if (port.direction == Input) {
            inputPorts += port.name
            recordName(port.name)
          }
          else if (port.direction == Output) {
            outputPorts += port.name
            recordName(port.name)
          }
        }
        else {
          /* We are processing a sub-module */
          nameToType(modulePrefix + "." + port.name) = port.tpe
          recordName(modulePrefix + "." + port.name)
          inlinedPorts += modulePrefix + "." + port.name
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
        if(! implementationFound) {
          println( s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
            """was not matched with an implementation""")
        }
    }
  }

  numberOfNodes = 0
  numberOfStatements = 0

  //    setVerbose(true)

  processModule("", module)

//  for(name <- validNames) {
//    if(! nameToExpression.contains(name)) {
//      val defaultValue = nameToType(name) match {
//        case UIntType(width) => UIntLiteral(0, width)
//        case SIntType(width) => SIntLiteral(0, width)
//        case ClockType       => UIntLiteral(0, IntWidth(1))
//        case _ =>
//          throw new Exception(s"error can't find default value for $name.type = ${nameToType(name)}")
//      }
//      nameToExpression(name) = defaultValue
//    }
//  }

  logger.debug(s"For module ${module.name} dependencyGraph =")
  nameToExpression.keys.toSeq.sorted foreach { k =>
    val v = nameToExpression(k).serialize
    logger.debug(s"  $k -> (" + v.toString.take(MaxColumnWidth) + ")")
  }

  def topologicalSort(stringToStrings: Map[String, Set[String]], strings: Iterable[String]): Iterable[String] = {
    @tailrec
    def innerSort(toPreds: Map[String, Set[String]], done: Iterable[String]): Iterable[String] = {
      println(s"Partion: $toPreds")
      val (noPreds, hasPreds) = toPreds.partition {
        _._2.isEmpty
      }
      if (noPreds.isEmpty) {
        if (hasPreds.isEmpty){
          done
        }
        else {
          val start = hasPreds.keys.head

          sys.error(hasPreds.toString)
        }
      } else {
        val found = noPreds.keys
        innerSort(hasPreds.mapValues {
          _ -- found
        }, done ++ found)
      }
    }

    val allSets:        Iterable[Set[String]] = stringToStrings.values
    val allValues:      Iterable[String]      = allSets.flatten
    val distinctValues: List[String]          = allValues.toList.distinct
    val newPairs:       List[(String, Set[String])] = distinctValues.flatMap { value =>
        if(stringToStrings.contains(value)) {
          None
        }
        else {
          Some(value -> Set.empty[String])
        }
      }
    println(s"New Pairs: $newPairs")
    val fullMap = stringToStrings ++ newPairs.toMap

    innerSort(fullMap, Seq())

  }

  for(key <- dependencies.keys.toSeq.sorted) {
    println(f"$key%-30s ${dependencies(key).toSeq.sorted.mkString(",")}")
  }

  val sorted: Iterable[String] = topologicalSort(dependencies.toMap, Seq())

  println(s"Sorted elements\n${sorted.mkString("\n")}")
  println(s"End of dependency graph")
  // scalastyle:on cyclomatic.complexity

}
