// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import firrtl_interpreter._


class ExpressionCompiler extends SimpleLogger {
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
              case Mul => MulInts(e1.apply, e2.apply)
              case Div => DivInts(e1.apply, e2.apply)
              case Rem => RemInts(e1.apply, e2.apply)

              case Eq  => EqInts(e1.apply, e2.apply)
              case Neq => NeqInts(e1.apply, e2.apply)
              case Lt  => LtInts(e1.apply, e2.apply)
              case Leq => LeqInts(e1.apply, e2.apply)
              case Gt  => GtInts(e1.apply, e2.apply)
              case Geq => GeqInts(e1.apply, e2.apply)

              case Dshl => DshlInts(e1.apply, e2.apply)
              case Dshr => DshrInts(e1.apply, e2.apply)

              case And  => AndBigs(e1.apply, e2.apply)
              case Or   => OrBigs(e1.apply, e2.apply)
              case Xor  => XorBigs(e1.apply, e2.apply)

              case Cat =>
                val width2 = args.tail.head.tpe match {
                  case GroundType(IntWidth(n)) => n.toInt
                  case _ => throw new InterpreterException(s"can't find width for Cat(${args.head}, ${args.tail.head})")
                }
                CatInts(e1.apply, e2.apply, width2)

              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
            }
          case (e1: BigExpressionResult, e2: IntExpressionResult) =>
            opCode match {
              case Add => AddBigs(e1.apply, ToBig(e2.apply).apply)
              case Sub => SubBigs(e1.apply, ToBig(e2.apply).apply)
              case Mul => MulBigs(e1.apply, ToBig(e2.apply).apply)
              case Div => DivBigs(e1.apply, ToBig(e2.apply).apply)
              case Rem => RemBigs(e1.apply, ToBig(e2.apply).apply)

              case Eq  => EqBigs(e1.apply, ToBig(e2.apply).apply)
              case Neq => NeqBigs(e1.apply, ToBig(e2.apply).apply)
              case Lt  => LtBigs(e1.apply, ToBig(e2.apply).apply)
              case Leq => LeqBigs(e1.apply, ToBig(e2.apply).apply)
              case Gt  => GtBigs(e1.apply, ToBig(e2.apply).apply)
              case Geq => GeqBigs(e1.apply, ToBig(e2.apply).apply)

              case Dshl => DshlBigs(e1.apply, ToBig(e2.apply).apply)
              case Dshr => DshrBigs(e1.apply, ToBig(e2.apply).apply)

              case And  => AndBigs(e1.apply, ToBig(e2.apply).apply)
              case Or   => OrBigs(e1.apply, ToBig(e2.apply).apply)
              case Xor  => XorBigs(e1.apply, ToBig(e2.apply).apply)

              case Cat =>
                val width2 = args.tail.head.tpe match {
                  case GroundType(IntWidth(n)) => n.toInt
                  case _ => throw new InterpreterException(s"can't find width for Cat(${args.head}, ${args.tail.head})")
                }
                CatBigs(e1.apply, ToBig(e2.apply).apply, width2)

              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head})")
            }
          case (e1: BigExpressionResult, e2: BigExpressionResult) =>
            opCode match {
              case Add => AddBigs(e1.apply, e2.apply)
              case Sub => SubBigs(e1.apply, e2.apply)
              case Mul => MulBigs(e1.apply, e2.apply)
              case Div => DivBigs(e1.apply, e2.apply)
              case Rem => RemBigs(e1.apply, e2.apply)

              case Eq  => EqBigs(e1.apply, e2.apply)
              case Neq => NeqBigs(e1.apply, e2.apply)
              case Lt  => LtBigs(e1.apply, e2.apply)
              case Leq => LeqBigs(e1.apply, e2.apply)
              case Gt  => GtBigs(e1.apply, e2.apply)
              case Geq => GeqBigs(e1.apply, e2.apply)

              case Dshl => DshlBigs(e1.apply, e2.apply)
              case Dshr => DshrBigs(e1.apply, e2.apply)

              case And  => AndBigs(e1.apply, e2.apply)
              case Or   => OrBigs(e1.apply, e2.apply)
              case Xor  => XorBigs(e1.apply, e2.apply)

              case Cat =>
                val width2 = args.tail.head.tpe match {
                  case GroundType(IntWidth(n)) => n.toInt
                  case _ => throw new InterpreterException(s"can't find width for Cat(${args.head}, ${args.tail.head})")
                }
                CatBigs(e1.apply, e2.apply, width2)

              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head})")
            }
          case _ =>
            throw InterpreterException(
              s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head}) ($arg1, $arg2)")
        }
      }

      def oneArgOneParamOps(
        op: PrimOp,
        expressions: Seq[Expression],
        ints: Seq[BigInt],
        tpe: firrtl.ir.Type
      ): ExpressionResult = {
        val arg1 = processExpression(expressions.head)
        val arg2 = ints.head
        val (isSigned, width) = tpe match {
          case UIntType(IntWidth(n)) => (false, n.toInt)
          case SIntType(IntWidth(n)) => (true, n.toInt)
        }

        arg1 match {
          case e1: IntExpressionResult =>
            op match {
              case Head => HeadInts(e1.apply, isSigned, arg2.toInt, width)
              case Tail => TailInts(e1.apply, isSigned, arg2.toInt, width)
              case Shl  => ShlInts(e1.apply, GetIntConstant(arg2.toInt).apply)
              case Shr  => ShrInts(e1.apply, GetIntConstant(arg2.toInt).apply)
            }
          case e1: BigExpressionResult =>
            op match {
              case Head => HeadBigs(e1.apply, isSigned, arg2.toInt, width)
              case Tail => TailBigs(e1.apply, isSigned, arg2.toInt, width)
              case Shl  => ShlBigs(e1.apply, GetBigConstant(arg2.toInt).apply)
              case Shr  => ShrBigs(e1.apply, GetBigConstant(arg2.toInt).apply)
            }
        }
      }

      def oneArgTwoParamOps(
        op: PrimOp,
        expressions: Seq[Expression],
        ints: Seq[BigInt],
        tpe: firrtl.ir.Type
      ): ExpressionResult = {
        val arg1 = processExpression(expressions.head)
        val arg2 = ints.head
        val arg3 = ints.tail.head
        val (isSigned, width) = tpe match {
          case UIntType(IntWidth(n)) => (false, n.toInt)
          case SIntType(IntWidth(n)) => (true, n.toInt)
        }

        arg1 match {
          case e1: IntExpressionResult =>
            op match {
              case Bits => BitsInts(e1.apply, isSigned, arg2.toInt, arg3.toInt, width)
            }
          case e1: BigExpressionResult =>
            op match {
              case Bits => BitsBigs(e1.apply, isSigned, arg2.toInt, arg3.toInt, width)
            }
        }
      }

      def unaryOps(
        op: PrimOp,
        expressions: Seq[Expression],
        tpe: firrtl.ir.Type
      ): ExpressionResult = {
        val arg1 = processExpression(expressions.head)
        val (isSigned, width) = tpe match {
          case UIntType(IntWidth(n)) => (false, n.toInt)
          case SIntType(IntWidth(n)) => (true, n.toInt)
        }

        arg1 match {
          case e1: IntExpressionResult =>
            op match {
              case Pad     => e1
              case AsUInt  => e1
              case AsSInt  => e1
              case AsClock => e1

              case Cvt     => e1
              case Neg     => NegInts(e1.apply)
              case Not     => NotInts(e1.apply)

              case Andr    => AndrInts(e1.apply, width)
              case Orr     => NotInts(e1.apply)
              case Xorr    => NegInts(e1.apply)
            }
          case e1: BigExpressionResult =>
            op match {
              case Pad     => e1
              case AsUInt  => e1
              case AsSInt  => e1
              case AsClock => e1

              case Cvt     => e1
              case Neg     => NegBigs(e1.apply)
              case Not     => NotBigs(e1.apply)

              case Andr    => AndrBigs(e1.apply, width)
              case Orr     => NotBigs(e1.apply)
              case Xorr    => NegBigs(e1.apply)
            }
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
            processExpression(condition) match {
              case c: IntExpressionResult =>
                (processExpression(trueExpression), processExpression(falseExpression)) match {
                  case (t: IntExpressionResult, f: IntExpressionResult) =>
                    MuxInts(c.apply, t.apply, f.apply)
                  case (t: BigExpressionResult, f: BigExpressionResult) =>
                    MuxBigs(c.apply, t.apply, f.apply)
                }
              case c =>
                throw InterpreterException(s"Mux condition is not 1 bit $condition parsed as $c")
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

              case Eq  => binaryOps(op, args, tpe)
              case Neq => binaryOps(op, args, tpe)
              case Lt  => binaryOps(op, args, tpe)
              case Leq => binaryOps(op, args, tpe)
              case Gt  => binaryOps(op, args, tpe)
              case Geq => binaryOps(op, args, tpe)

              case AsUInt  => unaryOps(op, args, tpe)
              case AsSInt  => unaryOps(op, args, tpe)
              case AsClock => unaryOps(op, args, tpe)

              case Shl => oneArgOneParamOps(op, args, const, tpe)
              case Shr => oneArgOneParamOps(op, args, const, tpe)

              case Dshl => binaryOps(op, args, tpe)
              case Dshr => binaryOps(op, args, tpe)

              case Cvt => unaryOps(op, args, tpe)
              case Neg => unaryOps(op, args, tpe)
              case Not => unaryOps(op, args, tpe)

              case And => binaryOps(op, args, tpe)
              case Or  => binaryOps(op, args, tpe)
              case Xor => binaryOps(op, args, tpe)

              case Andr => unaryOps(op, args, tpe)
              case Orr =>  unaryOps(op, args, tpe)
              case Xorr => unaryOps(op, args, tpe)

              case Cat => binaryOps(op, args, tpe)

              case Bits => oneArgTwoParamOps(op, args, const, tpe)

              case Head => oneArgOneParamOps(op, args, const, tpe)
              case Tail => oneArgOneParamOps(op, args, const, tpe)
              case _ =>
                throw new Exception(s"processExpression:error: unhandled expression $expression")
            }
            v
          case UIntLiteral(value, IntWidth(width)) =>
            if(Value.isBig(width.toInt))  GetBigConstant(value) else GetIntConstant(value.toInt)
          case SIntLiteral(value, IntWidth(width)) =>
            if(Value.isBig(width.toInt))  GetBigConstant(value) else GetIntConstant(value.toInt)
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
          // if it's a register we use the name of it's input side
          def renameIfRegister(name: String): String = {
            if (state.isRegister(name)) {
              s"$name${ExpressionCompiler.RegisterInputSuffix}"
            }
            else {
              name
            }
          }

          val newWireValue = con.loc match {
            case WRef(name, tpe, _, _) =>
              state.newValue(renameIfRegister(expand(name)), tpe)
            case (_: WSubField | _: WSubIndex) =>
              state.newValue(renameIfRegister(expand(con.loc.serialize)), con.loc.tpe)
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
          val resetResult = processExpression(resetExpression)
          val resetValue  = processExpression(initValueExpression)

          val registerIn  = state.newValue(s"$expandedName${ExpressionCompiler.RegisterInputSuffix}", tpe)
          val registerOut = state.newValue(s"$expandedName", tpe)

          state.registerNames += expandedName

          registerIn match {
            case e: BigValue =>
              state.triggeredAssign(clockResult, registerOut, GetBig(e))
              resetValue match {
                case rv: IntExpressionResult => state.triggeredAssign(resetResult, registerOut, ToBig(rv.apply))
                case rv: BigExpressionResult => state.triggeredAssign(resetResult, registerOut, rv)
              }
            case e: IntValue =>
              state.triggeredAssign(clockResult, registerOut, GetInt(e))
              resetValue match {
                case rv: IntExpressionResult => state.triggeredAssign(resetResult, registerOut, rv)
                case rv: BigExpressionResult => state.triggeredAssign(resetResult, registerOut, ToInt(rv.apply))
              }
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
        processStatements(module.body)
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
  def compile(circuit: Circuit): ExecutableCircuit = {
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

object ExpressionCompiler {
  val RegisterInputSuffix = "/in"
}
