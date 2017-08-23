//// See LICENSE for license details.
//
//package firrtl_interpreter.executable
//
//import firrtl._
//import firrtl.ir._
//import firrtl_interpreter._
//
//import scala.collection.mutable
//
//class ExpressionCompiler extends SimpleLogger {
//  val MaxColumnWidth = 100 // keeps displays of expressions readable
//
//  /**
//    * finds the specified module name in the circuit
//    *
//    * @param moduleName name to find
//    * @param circuit circuit being analyzed
//    * @return the circuit, exception occurs in not found
//    */
//  def findModule(moduleName: String, circuit: Circuit): DefModule = {
//    circuit.modules.find(module => module.name == moduleName) match {
//      case Some(module: Module) =>
//        module
//      case Some(externalModule: DefModule) =>
//        externalModule
//      case _ =>
//        throw InterpreterException(s"Could not find top level module in $moduleName")
//    }
//  }
//
//  val wires = new mutable.ArrayBuffer[WireValue]()
//  var nextIntIndex = -1
//  def getIntIndex(): Int = { nextIntIndex += 1; nextIntIndex}
//  var nextBigIntIndex = -1
//  def getBigIntIndex(): Int = { nextBigIntIndex += 1; nextBigIntIndex}
//  val executableExpressions = new mutable.ArrayBuffer[Assigner]
//
//  val state = ExecutableCircuit(Map.empty)
//
//  // scalastyle:off
//  def processModule(modulePrefix: String, myModule: DefModule, dependencyGraph: DependencyGraph): Unit = {
//    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name
//
//    def processDependencyStatements(s: firrtl.ir.Statement): firrtl.ir.Statement = {
//
//      def processExpression(expression: Expression): () => Int = {
//        dependencyGraph.numberOfNodes += 1
//        val result = expression match {
//          case Mux(condition, trueExpression, falseExpression, tpe) =>
//            dependencyGraph.numberOfMuxes += 1
//            Mux(
//              processExpression(condition),
//              processExpression(trueExpression),
//              processExpression(falseExpression),
//              tpe
//            )
//          case WRef(name, tpe, kind, gender) => WRef(expand(name), tpe, kind, gender)
//          case WSubField(subExpression, name, tpe, gender) =>
//            WSubField(processExpression(subExpression), name, tpe, gender)
//          case WSubIndex(subExpression, value, tpe, gender) =>
//            WSubIndex(processExpression(subExpression), value, tpe, gender)
//          case ValidIf(condition, value, tpe) => ValidIf(processExpression(condition), processExpression(value), tpe)
//          case DoPrim(op, args, const, tpe) =>
//            DoPrim(op, args.map { subExpression => processExpression(subExpression)}, const, tpe)
//          case c: UIntLiteral => c
//          case c: SIntLiteral => c
//          case _ =>
//            throw new Exception(s"processExpression:error: unhandled expression $expression")
//        }
//        result
//      }
//
//      dependencyGraph.numberOfStatements += 1
//      s match {
//        case block: Block =>
//          block.stmts.map { subStatement =>
//            processDependencyStatements(subStatement)
//          }
//          block
//        case con: Connect =>
//          val newWireValue = con.loc match {
//            case WRef(name, tpe, _, _) => makeWire(expand(name), tpe)
//            case (_: WSubField | _: WSubIndex) => makeWire(expand(con.loc.serialize), con.loc.tpe)
//          }
//          executableExpressions += AssignInt(state, newWireValue.index, processExpression(con.expr))
//
//          con
//        case WDefInstance(info, instanceName, moduleName, _) =>
//          val subModule = findModule(moduleName, dependencyGraph.circuit)
//          val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
//          log(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
//          processModule(newPrefix, subModule, dependencyGraph)
//          dependencyGraph.addSourceInfo(newPrefix, info)
//          dependencyGraph.addInstanceName(instanceName, moduleName)
//          s
//        case DefNode(info, name, expression) =>
//          log(s"declaration:DefNode:$name:${expression.serialize} ${processExpression(expression).serialize}")
//          val expandedName = expand(name)
//          dependencyGraph.nodes += expandedName
//          dependencyGraph.recordName(expandedName)
//          dependencyGraph(expandedName) = processExpression(expression)
//          dependencyGraph.addSourceInfo(expandedName, info)
//          s
//        case DefWire(info, name, tpe) =>
//          log(s"declaration:DefWire:$name")
//          val expandedName = expand(name)
//          dependencyGraph.wires += expandedName
//          dependencyGraph.recordName(expandedName)
//          dependencyGraph.recordType(expandedName, tpe)
//          dependencyGraph.addSourceInfo(expandedName, info)
//          s
//        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
//          log(s"declaration:DefRegister:$name clock <- ${clockExpression.serialize} ${processExpression(clockExpression).serialize}")
//          log(s"declaration:DefRegister:$name reset <- ${resetExpression.serialize} ${processExpression(resetExpression).serialize}")
//          log(s"declaration:DefRegister:$name init  <- ${initValueExpression.serialize} ${processExpression(initValueExpression).serialize}")
//          val renamedDefRegister = DefRegister(
//            info, expand(name), tpe,
//            processExpression(clockExpression),
//            processExpression(resetExpression),
//            processExpression(initValueExpression)
//          )
//          val expandedName = expand(name)
//          dependencyGraph.registerNames += expandedName
//          dependencyGraph.recordName(expandedName)
//          dependencyGraph.recordType(expandedName, tpe)
//          dependencyGraph.registers(expandedName) = renamedDefRegister
//          dependencyGraph.addSourceInfo(expandedName, info)
//          s
//        case defMemory: DefMemory =>
//          val expandedName = expand(defMemory.name)
//          log(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
//          val newDefMemory = defMemory.copy(name = expandedName)
//          dependencyGraph.addMemory(newDefMemory)
//          dependencyGraph.addSourceInfo(expandedName, defMemory.info)
//          s
//        case IsInvalid(info, expression) =>
//          IsInvalid(info, processExpression(expression))
//        case Stop(info, ret, clkExpression, enableExpression) =>
//          dependencyGraph.addStop(Stop(info, ret, processExpression(clkExpression), processExpression(enableExpression)))
//          s
//        case Print(info, stringLiteral, argExpressions, clkExpression, enableExpression) =>
//          dependencyGraph.addPrint(Print(
//            info, stringLiteral,
//            argExpressions.map { expression => processExpression(expression) },
//            processExpression(clkExpression),
//            processExpression(enableExpression)
//          ))
//          s
//        case EmptyStmt =>
//          s
//        case conditionally: Conditionally =>
//          // log(s"got a conditionally $conditionally")
//          throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
//        case _ =>
//          println(s"TODO: Unhandled statement $s")
//          s
//      }
//    }
//    // scalastyle:on
//
//    def processExternalInstance(extModule: ExtModule,
//                                modulePrefix: String,
//                                instance: BlackBoxImplementation,
//                                dependencyGraph: DependencyGraph): Unit = {
//      def expand(name: String): String = modulePrefix + "." + name
//
//      for(port <- extModule.ports) {
//        if(port.direction == Output) {
//          val outputDependencies = instance.outputDependencies(port.name)
//          dependencyGraph(expand(port.name)) = BlackBoxOutput(port.name, instance, outputDependencies, port.tpe)
//        }
//      }
//    }
//
//    def getBitWidth(tpe: Type): Int = {
//      tpe match {
//        case UIntType(IntWidth(n)) => n.toInt
//        case SIntType(IntWidth(n)) => n.toInt
//        case _ =>
//          throw InterpreterException(s"bad tpe: $tpe")
//      }
//    }
//
//    def getIsSigned(tpe: Type): Boolean = {
//      tpe match {
//        case _: UIntType => false
//        case _: SIntType => true
////        case _: firrtl.ir.ClockType => false
//        case _ =>
//          throw InterpreterException(s"bad tpe: $tpe")
//      }
//    }
//
//    def makeWire(name: String, tpe: Type): WireValue = {
//      val bitWidth = getBitWidth(tpe)
//      val newIndex = if(bitWidth > 32) { getBigIntIndex() } else { getIntIndex() }
//
//      val wireValue = WireValue(name, getIsSigned(tpe),bitWidth, newIndex)
//      wireValue
//    }
//
//    def processPorts(module: DefModule): Unit = {
//      for(port <- module.ports) {
//        val newWireValue = makeWire(expand(port.name), port.tpe)
//        wires += newWireValue
//      }
//    }
//
//    myModule match {
//      case module: firrtl.ir.Module =>
//        processPorts(module)
//        processDependencyStatements(module.body, dependencyGraph)
//      case extModule: ExtModule => // Look to see if we have an implementation for this
//        log(s"got external module ${extModule.name} instance $modulePrefix")
//        processPorts(extModule)
//        /* use exists while looking for the right factory, short circuits iteration when found */
//        log(s"Factories: ${dependencyGraph.blackBoxFactories.mkString("\n")}")
//        val implementationFound = dependencyGraph.blackBoxFactories.exists { factory =>
//          log("Found an existing factory")
//          factory.createInstance(modulePrefix, extModule.defname) match {
//            case Some(implementation) =>
//              processExternalInstance(extModule, modulePrefix, implementation, dependencyGraph)
//              true
//            case _ => false
//          }
//        }
//        if(! implementationFound) {
//          println( s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
//            """was not matched with an implementation""")
//        }
//    }
//  }
//
//  // scalastyle:off cyclomatic.complexity
//  def compile(circuit: Circuit, interpreter: FirrtlTerp): DependencyGraph = {
//    val module = findModule(circuit.main, circuit) match {
//      case regularModule: Module => regularModule
//      case externalModule: ExtModule =>
//        throw InterpreterException(s"Top level module must be a regular module $externalModule")
//      case x =>
//        throw InterpreterException(s"Top level module is not the right kind of module $x")
//    }
//
//    val dependencyGraph = new DependencyGraph(circuit, module, interpreter.blackBoxFactories)
//
//    processModule("", module, dependencyGraph)
//
//    for(name <- dependencyGraph.validNames) {
//      if(! dependencyGraph.nameToExpression.contains(name)) {
//        val defaultValue = dependencyGraph.nameToType(name) match {
//          case UIntType(width) => UIntLiteral(0, width)
//          case SIntType(width) => SIntLiteral(0, width)
//          case ClockType       => UIntLiteral(0, IntWidth(1))
//          case _ =>
//            throw new Exception(s"error can't find default value for $name.type = ${dependencyGraph.nameToType(name)}")
//        }
//        dependencyGraph.nameToExpression(name) = defaultValue
//      }
//    }
//
//    log(s"For module ${module.name} dependencyGraph =")
//    dependencyGraph.nameToExpression.keys.toSeq.sorted foreach { k =>
//      val v = dependencyGraph.nameToExpression(k).serialize
//      log(s"  $k -> (" + v.toString.take(MaxColumnWidth) + ")")
//    }
//    println(s"End of dependency graph")
//    dependencyGraph
//  }
//}
