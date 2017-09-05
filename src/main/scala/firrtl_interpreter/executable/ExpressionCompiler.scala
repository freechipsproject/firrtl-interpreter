// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import firrtl_interpreter._


class ExpressionCompiler extends SimpleLogger {
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
      case Some(module: firrtl.ir.Module) =>
        module
      case Some(externalModule: firrtl.ir.ExtModule) =>
        externalModule
      case _ =>
        throw InterpreterException(s"Could not find top level module in $moduleName")
    }
  }

  val state = new ExecutableCircuit

  // scalastyle:off
  def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def processStatements(statement: firrtl.ir.Statement): Unit = {

      def binaryOps(opCode: PrimOp, args: Seq[Expression], tpe: Type): ExpressionResult = {
        val arg1 = processExpression(args.head)
        val arg2 = processExpression(args.tail.head)

        (arg1, arg2) match {
          case (e1: IntExpressionResult, e2: IntExpressionResult) =>
            opCode match {
              case Add => AddInts(e1.apply, e2.apply)
              case Sub => SubInts(e1.apply, e2.apply)
              case Eq  => EqInts(e1.apply, e2.apply)
              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
            }
          case (e1: BigExpressionResult, e2: BigExpressionResult) =>
            opCode match {
              case Add => AddBigs(e1.apply, e2.apply)
              case Sub => SubBigs(e1.apply, e2.apply)
              case Eq  => EqBigs(e1.apply, e2.apply)
              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
            }
          case _ =>
            throw InterpreterException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
        }
      }

      /**
        * Process loFirrtl expression and return an executable result
        *
        * @param expression a loFirrtlExpression
        * @return
        */
      def processExpression(expression: Expression): ExpressionResult = {
        val result: ExpressionResult = expression match {
          case Mux(condition, trueExpression, falseExpression, _) =>
            (processExpression(condition), processExpression(trueExpression), processExpression(falseExpression)) match {
              case (c: IntExpressionResult, t: IntExpressionResult, f: IntExpressionResult) =>
                MuxInts(c.apply, t.apply, f.apply)
              case (c: BigExpressionResult, t: BigExpressionResult, f: BigExpressionResult) =>
                MuxBigs(c.apply, t.apply, f.apply)
            }
          case WRef(name, tpe, kind, gender) =>
            state.newValue(name, tpe) match {
              case v: IntValue => GetInt(v)
              case v: BigValue => GetBig(v)
            }
          case subfield: WSubField =>
            state.newValue(subfield.serialize, subfield.tpe) match {
              case v: IntValue => GetInt(v)
              case v: BigValue => GetBig(v)
            }
          case subIndex: WSubIndex =>
            state.newValue(subIndex.serialize, subIndex.tpe) match {
              case v: IntValue => GetInt(v)
              case v: BigValue => GetBig(v)
            }
          //          TODO:(chick) case ValidIf(condition, value, tpe) => ValidIf(processExpression(condition), processExpression(value), tpe)
          case DoPrim(op, args, const, tpe) =>
            val v = op match {
              case Add => binaryOps(op, args, tpe)
              case Sub => binaryOps(op, args, tpe)
              case Mul => binaryOps(op, args, tpe)
              case Div => binaryOps(op, args, tpe)
              case Rem => binaryOps(op, args, tpe)

              case Eq => binaryOps(op, args, tpe)
              case Neq => binaryOps(op, args, tpe)
              case Lt => binaryOps(op, args, tpe)
              case Leq => binaryOps(op, args, tpe)
              case Gt => binaryOps(op, args, tpe)
              case Geq => binaryOps(op, args, tpe)
              case _ =>
                throw new Exception(s"processExpression:error: unhandled expression $expression")
            }
            v
          case UIntLiteral(value, IntWidth(width)) =>
            if(Value.isBig(width.toInt)) GetIntConstant(value.toInt) else GetBigConstant(value)
          case SIntLiteral(value, IntWidth(width)) =>
            if(Value.isBig(width.toInt)) GetIntConstant(value.toInt) else GetBigConstant(value)
          case _ =>
            throw new InterpreterException(s"bad expression $expression")
        }
        result
      }

      statement match {
        case block: Block =>
          block.stmts.foreach { subStatement =>
            processStatements(subStatement)
          }
        case con: Connect =>
          val newWireValue = con.loc match {
            case WRef(name, tpe, _, _) => state.newValue(expand(name), tpe)
            case (_: WSubField | _: WSubIndex) => state.newValue(expand(con.loc.serialize), con.loc.tpe)
          }
          state.assign(newWireValue, processExpression(con.expr))
        case WDefInstance(info, instanceName, moduleName, _) =>
          val subModule = findModule(moduleName, circuit)
          val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
          log(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
          processModule(newPrefix, subModule, circuit)
        case DefNode(info, name, expression) =>
          log(s"declaration:DefNode:$name:${expression.serialize}")

          processExpression(expression) match {
            case e: IntExpressionResult =>
              val value = state.newValue(expand(name), isSigned = true, width = Value.BigThreshold)  //TODO:(chick) compute
              state.assign(value, e)
            case e: BigExpressionResult =>
              val value = state.newValue(expand(name), isSigned = true, width = Value.BigThreshold * 2) //TODO:(chick) compute
              state.assign(value, e)
            case _ =>
              throw InterpreterException(s"bad expression $expression")
          }
        case DefWire(info, name, tpe) =>
          log(s"declaration:DefWire:$name")
          state.newValue(expand(name), tpe)
        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
          log(s"declaration:DefRegister:$name")
//          log(s"declaration:DefRegister:$name clock <- ${clockExpression.serialize} ${processExpression(clockExpression).serialize}")
//          log(s"declaration:DefRegister:$name reset <- ${resetExpression.serialize} ${processExpression(resetExpression).serialize}")
//          log(s"declaration:DefRegister:$name init  <- ${initValueExpression.serialize} ${processExpression(initValueExpression).serialize}")
          val expandedName = expand(name)

          val clockResult = processExpression(clockExpression)

          val registerIn  = state.newValue(s"$expandedName/in", tpe)
          val registerOut = state.newValue(s"$expandedName", tpe)

          registerIn match {
            case e: BigValue => state.clockAssign(clockResult, registerOut, GetBig(e))
            case e: IntValue => state.clockAssign(clockResult, registerOut, GetInt(e))
            case _ =>
              throw InterpreterException(s"bad register $statement")
          }
        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          log(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
          val newDefMemory = defMemory.copy(name = expandedName)
        case IsInvalid(info, expression) =>
//          IsInvalid(info, processExpression(expression))
        case Stop(info, ret, clkExpression, enableExpression) =>
//          dependencyGraph.addStop(Stop(info, ret, processExpression(clkExpression), processExpression(enableExpression)))
//          s
        case Print(info, stringLiteral, argExpressions, clkExpression, enableExpression) =>
//          dependencyGraph.addPrint(Print(
//            info, stringLiteral,
//            argExpressions.map { expression => processExpression(expression) },
//            processExpression(clkExpression),
//            processExpression(enableExpression)
//          ))
//          s
        case EmptyStmt =>
        case conditionally: Conditionally =>
          // log(s"got a conditionally $conditionally")
          throw new InterpreterException(s"conditionally unsupported in interpreter $conditionally")
        case _ =>
          println(s"TODO: Unhandled statement $statement")
      }
    }
    // scalastyle:on

    def processExternalInstance(extModule: ExtModule,
                                modulePrefix: String,
                                instance: BlackBoxImplementation,
                                dependencyGraph: DependencyGraph): Unit = {
      def expand(name: String): String = modulePrefix + "." + name

      for(port <- extModule.ports) {
        if(port.direction == Output) {
          val outputDependencies = instance.outputDependencies(port.name)
          dependencyGraph(expand(port.name)) = BlackBoxOutput(port.name, instance, outputDependencies, port.tpe)
        }
      }
    }

    def getBitWidth(tpe: Type): Int = {
      tpe match {
        case UIntType(IntWidth(n)) => n.toInt
        case SIntType(IntWidth(n)) => n.toInt
        case _ =>
          throw InterpreterException(s"bad tpe: $tpe")
      }
    }

    def getIsSigned(tpe: Type): Boolean = {
      tpe match {
        case _: UIntType => false
        case _: SIntType => true
//        case _: firrtl.ir.ClockType => false
        case _ =>
          throw InterpreterException(s"bad tpe: $tpe")
      }
    }

    def processPorts(module: DefModule): Unit = {
      for(port <- module.ports) {
        state.newValue(expand(port.name), port.tpe)
      }
    }

    myModule match {
      case module: firrtl.ir.Module =>
        processPorts(module)
      case extModule: ExtModule => // Look to see if we have an implementation for this
        log(s"got external module ${extModule.name} instance $modulePrefix")
        processPorts(extModule)
        /* use exists while looking for the right factory, short circuits iteration when found */
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
    }
  }

  // scalastyle:off cyclomatic.complexity
  def compile(circuit: Circuit, interpreter: FirrtlTerp): ExecutableCircuit = {
    val module = findModule(circuit.main, circuit) match {
      case regularModule: firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw InterpreterException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw InterpreterException(s"Top level module is not the right kind of module $x")
    }


    processModule("", module, circuit)

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
    state
  }
}
