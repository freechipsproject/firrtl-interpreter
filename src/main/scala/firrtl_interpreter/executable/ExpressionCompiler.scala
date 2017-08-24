//// See LICENSE for license details.
//
//package firrtl_interpreter.executable
//
//import firrtl.PrimOps._
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
//  val executableExpressions = new mutable.ArrayBuffer[Assigner]
//
//  val state = ExecutableCircuit(Map.empty)
//
//  // scalastyle:off
//  def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
//    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name
//
//    def processDependencyStatements(s: firrtl.ir.Statement): Unit = {
//
//      def mathPrimitive(opCode: PrimOp, args: Seq[Expression], tpe: Type): () => Int = {
//        val arg1 = processExpression(args.head)
//        val arg2 = processExpression(args.tail.head)
//        opCode match {
//          case Add => AddInts(arg1, arg2).apply
//          case Sub => SubInts(arg1, arg2).apply
//            //TODO:(chick) build this out
////          case Mul => arg1 * arg2
////          case Div => arg1 / arg2
////          case Rem => arg1 % arg2
//        }
//      }
//
//      def comparisonOp(opCode: PrimOp, args: Seq[Expression], tpe: Type): () => Int = {
//        val arg1 = processExpression(args.head)
//        val arg2 = processExpression(args.tail.head)
//        opCode match {
//          case Eq      => EqInts(arg1, arg2).apply
//          //TODO:(chick) build this out
////          case Neq     => arg1 != arg2
////          case Lt      => arg1 <  arg2
////          case Leq     => arg1 <= arg2
//          case Gt      => GtInts(arg1, arg2).apply
////          case Geq => arg1 >= arg2
//        }
//      }
//      def processExpression(expression: Expression): () => Int = {
//        val result: () => Int = expression match {
//          case Mux(condition, trueExpression, falseExpression, tpe) =>
//            MuxInts(
//              processExpression(condition),
//              processExpression(trueExpression),
//              processExpression(falseExpression)
//            ).apply
//          case WRef(name, tpe, kind, gender) =>
//            val getInt = GetInt(state, state.getUInt(expand(name)))
//            getInt.apply
//          case subfield: WSubField =>
//            val getInt = GetInt(state, state.getUInt(expand(subfield.serialize)))
//            getInt.apply
//          case subindex: WSubIndex =>
//            val getInt = GetInt(state, state.getUInt(expand(subindex.serialize)))
//            getInt.apply
//          //          TODO:(chick) case ValidIf(condition, value, tpe) => ValidIf(processExpression(condition), processExpression(value), tpe)
//          case DoPrim(op, args, const, tpe) =>
//            val v = op match {
//              case Add => mathPrimitive(op, args, tpe)
//              case Sub => mathPrimitive(op, args, tpe)
//              case Mul => mathPrimitive(op, args, tpe)
//              case Div => mathPrimitive(op, args, tpe)
//              case Rem => mathPrimitive(op, args, tpe)
//
//              case Eq => comparisonOp(op, args, tpe)
//              case Neq => comparisonOp(op, args, tpe)
//              case Lt => comparisonOp(op, args, tpe)
//              case Leq => comparisonOp(op, args, tpe)
//              case Gt => comparisonOp(op, args, tpe)
//              case Geq => comparisonOp(op, args, tpe)
//              case c: UIntLiteral => GetIntConstant(c.value.toInt).apply
//              case c: SIntLiteral => GetIntConstant(c.value.toInt).apply
//              case _ =>
//                throw new Exception(s"processExpression:error: unhandled expression $expression")
//            }
//            result
//          case _ =>
//            throw new InterpreterException(s"bad expression $expression")
//        }
//        result
//      }
//
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
//          state.combinationalExpressions += AssignInt(state, newWireValue, processExpression(con.expr))
//        case WDefInstance(info, instanceName, moduleName, _) =>
//          val subModule = findModule(moduleName, circuit)
//          val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
//          log(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
//          processModule(newPrefix, subModule, circuit)
//        case DefNode(info, name, expression) =>
//          log(s"declaration:DefNode:$name:${expression.serialize}")
//          val newWireValue = makeWire(expand(name), 32)  //TODO:(chick) This width cannot be hard-coded
//          executableExpressions += AssignInt(state, newWireValue, processExpression(expression))
//        case DefWire(info, name, tpe) =>
//          log(s"declaration:DefWire:$name")
//          makeWire(expand(name), tpe)
//        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
//          log(s"declaration:DefRegister:$name")
////          log(s"declaration:DefRegister:$name clock <- ${clockExpression.serialize} ${processExpression(clockExpression).serialize}")
////          log(s"declaration:DefRegister:$name reset <- ${resetExpression.serialize} ${processExpression(resetExpression).serialize}")
////          log(s"declaration:DefRegister:$name init  <- ${initValueExpression.serialize} ${processExpression(initValueExpression).serialize}")
//          val expandedName = expand(name)
//          val newWireValueIn = makeWire(s"${expandedName}/in", tpe)
//          val newWireValueOut = makeWire(s"${expandedName}", tpe)
//
//          state.registerExpressions += AssignInt(state, newWireValueOut, GetInt(state, newWireValueIn).apply)
//        case defMemory: DefMemory =>
//          val expandedName = expand(defMemory.name)
//          log(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
//          val newDefMemory = defMemory.copy(name = expandedName)
//        case IsInvalid(info, expression) =>
////          IsInvalid(info, processExpression(expression))
//        case Stop(info, ret, clkExpression, enableExpression) =>
////          dependencyGraph.addStop(Stop(info, ret, processExpression(clkExpression), processExpression(enableExpression)))
////          s
//        case Print(info, stringLiteral, argExpressions, clkExpression, enableExpression) =>
////          dependencyGraph.addPrint(Print(
////            info, stringLiteral,
////            argExpressions.map { expression => processExpression(expression) },
////            processExpression(clkExpression),
////            processExpression(enableExpression)
////          ))
////          s
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
//    def makeWire(name: String, tpe: Type): executable.UInt = {
//      makeWire(name, getBitWidth(tpe))
//    }
//
//    def makeWire(name: String, width: Int): executable.UInt = {
//      val wireValue = executable.UInt(name, width)
//      state.addWire(wireValue)
//      wireValue
//    }
//
//    def processPorts(module: DefModule): Unit = {
//      for(port <- module.ports) {
//        val newWireValue = makeWire(expand(port.name), port.tpe)
//        state.addWire(newWireValue)
//      }
//    }
//
//    myModule match {
//      case module: firrtl.ir.Module =>
//        processPorts(module)
//      case extModule: ExtModule => // Look to see if we have an implementation for this
//        log(s"got external module ${extModule.name} instance $modulePrefix")
//        processPorts(extModule)
//        /* use exists while looking for the right factory, short circuits iteration when found */
////        log(s"Factories: ${dependencyGraph.blackBoxFactories.mkString("\n")}")
////        val implementationFound = dependencyGraph.blackBoxFactories.exists { factory =>
////          log("Found an existing factory")
////          factory.createInstance(modulePrefix, extModule.defname) match {
////            case Some(implementation) =>
////              processExternalInstance(extModule, modulePrefix, implementation, dependencyGraph)
////              true
////            case _ => false
////          }
////        }
////        if(! implementationFound) {
////          println( s"""WARNING: external module "${extModule.defname}"($modulePrefix:${extModule.name})""" +
////            """was not matched with an implementation""")
////        }
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
//
//    processModule("", module, circuit)
//
////    for(name <- dependencyGraph.validNames) {
////      if(! dependencyGraph.nameToExpression.contains(name)) {
////        val defaultValue = dependencyGraph.nameToType(name) match {
////          case UIntType(width) => UIntLiteral(0, width)
////          case SIntType(width) => SIntLiteral(0, width)
////          case ClockType       => UIntLiteral(0, IntWidth(1))
////          case _ =>
////            throw new Exception(s"error can't find default value for $name.type = ${dependencyGraph.nameToType(name)}")
////        }
////        dependencyGraph.nameToExpression(name) = defaultValue
////      }
////    }
////
////    log(s"For module ${module.name} dependencyGraph =")
////    dependencyGraph.nameToExpression.keys.toSeq.sorted foreach { k =>
////      val v = dependencyGraph.nameToExpression(k).serialize
////      log(s"  $k -> (" + v.toString.take(MaxColumnWidth) + ")")
////    }
////    println(s"End of dependency graph")
////    dependencyGraph
//  }
//}
