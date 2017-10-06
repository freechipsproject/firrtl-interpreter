// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import firrtl_interpreter._


class ExpressionCompiler(program: Program) extends logger.LazyLogging {
  val dataStore:   DataStore   = program.dataStore
  val symbolTable: SymbolTable = program.symbolTable
  val scheduler:   Scheduler   = program.scheduler

  def getWidth(tpe: firrtl.ir.Type): Int = {
    tpe match {
      case GroundType(IntWidth(width)) => width.toInt
      case _ => throw new InterpreterException(s"Unresolved width found in firrtl.ir.Type $tpe")
    }
  }

  def getWidth(expression: Expression): Int = {
    expression.tpe match {
      case GroundType(IntWidth(width)) => width.toInt
      case _ =>
        throw new InterpreterException(
          s"Unresolved width found in expression $expression of firrtl.ir.Type ${expression.tpe}")
    }
  }

  def getSigned(expression: Expression): Boolean = {
    expression.tpe match {
      case  _: UIntType    => false
      case  _: SIntType    => true
      case  ClockType      => false
      case _ =>
        throw new InterpreterException(
          s"Unsupported type found in expression $expression of firrtl.ir.Type ${expression.tpe}")
    }
  }

  // scalastyle:off
  def processModule(modulePrefix: String, myModule: DefModule, circuit: Circuit): Unit = {
    def expand(name: String): String = if(modulePrefix.isEmpty) name else modulePrefix + "." + name

    def processStatements(statement: firrtl.ir.Statement): Unit = {

      def binaryOps(opCode: PrimOp, args: Seq[Expression], tpe: Type): ExpressionResult = {

        def getParameters(e: Expression) = (processExpression(e), getSigned(e), getWidth(e))

        val (arg1, arg1IsSigned, arg1Width) = getParameters(args.head)
        val (arg2, arg2IsSigned, arg2Width) = getParameters(args.tail.head)

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

              case And  => AndInts(e1.apply, e2.apply)
              case Or   => OrInts(e1.apply, e2.apply)
              case Xor  => XorInts(e1.apply, e2.apply)

              case Cat =>
                CatInts(e1.apply, arg1IsSigned, arg1Width, e2.apply, arg2IsSigned, arg2Width)

              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
            }
          case (e1: IntExpressionResult, e2: BigExpressionResult) =>
            opCode match {
              case Add => AddBigs(ToBig(e1.apply).apply, e2.apply)
              case Sub => SubBigs(ToBig(e1.apply).apply, e2.apply)
              case Mul => MulBigs(ToBig(e1.apply).apply, e2.apply)
              case Div => DivBigs(ToBig(e1.apply).apply, e2.apply)
              case Rem => RemBigs(ToBig(e1.apply).apply, e2.apply)

              case Eq  => EqBigs(ToBig(e1.apply).apply, e2.apply)
              case Neq => NeqBigs(ToBig(e1.apply).apply, e2.apply)
              case Lt  => LtBigs(ToBig(e1.apply).apply, e2.apply)
              case Leq => LeqBigs(ToBig(e1.apply).apply, e2.apply)
              case Gt  => GtBigs(ToBig(e1.apply).apply, e2.apply)
              case Geq => GeqBigs(ToBig(e1.apply).apply, e2.apply)

              case Dshl => DshlBigs(ToBig(e1.apply).apply, e2.apply)
              case Dshr => DshrBigs(ToBig(e1.apply).apply, e2.apply)

              case And  => AndBigs(ToBig(e1.apply).apply, e2.apply)
              case Or   => OrBigs(ToBig(e1.apply).apply, e2.apply)
              case Xor  => XorBigs(ToBig(e1.apply).apply, e2.apply)

              case Cat =>
                CatBigs(ToBig(e1.apply).apply, arg1IsSigned, arg1Width, e2.apply, arg2IsSigned, arg2Width)

              case _ =>
                throw InterpreterException(s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head})")
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
                CatBigs(e1.apply, arg1IsSigned, arg1Width, ToBig(e2.apply).apply, arg2IsSigned, arg2Width)

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
                CatBigs(e1.apply, arg1IsSigned, arg1Width, e2.apply, arg2IsSigned, arg2Width)

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
        val arg1Width = getWidth(expressions.head)
        val isSigned = getSigned(expressions.head)
        val param1 = ints.head.toInt

        arg1 match {
          case e1: IntExpressionResult =>
            op match {
              case Head => HeadInts(e1.apply, isSigned, takeBits = param1, arg1Width)
              case Tail => TailInts(e1.apply, isSigned, toDrop = param1, arg1Width)
              case Shl  => ShlInts(e1.apply, GetIntConstant(param1).apply)
              case Shr  => ShrInts(e1.apply, GetIntConstant(param1).apply)
            }
          case e1: BigExpressionResult =>
            op match {
              case Head => HeadBigs(e1.apply, isSigned, takeBits = param1, arg1Width)
              case Tail => TailBigs(e1.apply, isSigned, toDrop = param1, arg1Width)
              case Shl  => ShlBigs(e1.apply, GetBigConstant(param1).apply)
              case Shr  => ShrBigs(e1.apply, GetBigConstant(param1).apply)
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
          case ClockType             => (false, 1)
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
        def createAccessor(name: String): ExpressionResult = {
          val symbol = symbolTable(name)
          symbol.dataSize match {
            case IntSize => dataStore.GetInt(symbol.index)
//            case LongSize => dataStore.GetLong(symbol.index)
            case BigSize => dataStore.GetBig(symbol.index)
          }
        }

        val result: ExpressionResult = expression match {
          case Mux(condition, trueExpression, falseExpression, _) =>
            processExpression(condition) match {
              case c: IntExpressionResult =>
                (processExpression(trueExpression), processExpression(falseExpression)) match {
                  case (t: IntExpressionResult, f: IntExpressionResult) =>
                    MuxInts(c.apply, t.apply, f.apply)
                  case (t: BigExpressionResult, f: IntExpressionResult) =>
                    MuxBigs(c.apply, t.apply, ToBig(f.apply).apply)
                  case (t: IntExpressionResult, f: BigExpressionResult) =>
                    MuxBigs(c.apply, ToBig(t.apply).apply, f.apply)
                  case (t: BigExpressionResult, f: BigExpressionResult) =>
                    MuxBigs(c.apply, t.apply, f.apply)
                }
              case c =>
                throw InterpreterException(s"Mux condition is not 1 bit $condition parsed as $c")
            }

          case WRef(name, tpe, _, _) =>
            createAccessor(name)
          case subfield: WSubField =>
            createAccessor(subfield.serialize)
          case subIndex: WSubIndex =>
            createAccessor(subIndex.serialize)

          case ValidIf(condition, value, tpe) =>
            processExpression(condition) match {
              case c: IntExpressionResult =>
                processExpression(value) match {
                  case t: IntExpressionResult =>
                    MuxInts(c.apply, t.apply, UndefinedInts(getWidth(tpe)).apply)
                  case t: BigExpressionResult =>
                    MuxBigs(c.apply, t.apply, UndefinedBigs(getWidth(tpe)).apply)
                  case _ =>
                    throw InterpreterException(s"Mux condition is not 1 bit $condition parsed as $c")
                }
              case c =>
                throw InterpreterException(s"Mux condition is not 1 bit $condition parsed as $c")
            }
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
            DataSize(width) match {
              case IntSize => GetIntConstant(value.toInt)
              //              case LongSize => GetLongConstant(value.toInt)
              case BigSize => GetBigConstant(value.toInt)
            }
          case SIntLiteral(value, IntWidth(width)) =>
            DataSize(width) match {
              case IntSize => GetIntConstant(value.toInt)
              //              case LongSize => GetLongConstant(value.toInt)
              case BigSize => GetBigConstant(value.toInt)
            }          case _ =>
            throw new InterpreterException(s"bad expression $expression")
        }
        result
      }

      def getAssigner(name: String, expressionResult: ExpressionResult): Assigner = {
        val symbol = symbolTable(name)
        val assigner = (symbol.dataSize, expressionResult) match {
          case (IntSize, result: IntExpressionResult) => dataStore.AssignInt(symbol.index, result.apply)
//          case (LongSize, result: LongExpressionResult) => dataStore.AssignLong(symbol.index, result.apply)
          case (BigSize, result: IntExpressionResult) => dataStore.AssignBig(symbol.index, ToBig(result.apply).apply)
          case (BigSize, result: BigExpressionResult) => dataStore.AssignBig(symbol.index, result.apply)
          case (size, res) =>
            println(s"Yikes how am i here. $size, $res")
            ???
        }
        scheduler.combinationalAssigns += assigner
        assigner
      }

      def triggeredAssign(
                           triggerExpression: ExpressionResult,
                           value: Symbol,
                           expressionResult: ExpressionResult
                         ): Unit = {
        val assignment = (value.dataSize, expressionResult) match {
          case (IntSize, e: IntExpressionResult) => dataStore.AssignInt(value.index, e.apply)
//          case (LongSize, e: LongExpressionResult) => dataStore.AssignLong(value.index, e.apply)
          case (BigSize, e: BigExpressionResult) => dataStore.AssignBig(value.index, e.apply)
        }
        scheduler.triggeredAssigns(triggerExpression) += assignment
      }

      statement match {
        case block: Block =>
          block.stmts.foreach { subStatement =>
            processStatements(subStatement)
          }

        case con: Connect =>
          // if it's a register we use the name of its input side
          def renameIfRegister(name: String): String = {
            if (symbolTable.isRegister(name)) {
              s"$name${ExpressionCompiler.RegisterInputSuffix}"
            }
            else {
              name
            }
          }
          val lhsName = renameIfRegister(con.loc.serialize)
          getAssigner(lhsName, processExpression(con.expr))

        case WDefInstance(info, instanceName, moduleName, _) =>
          val subModule = FindModule(moduleName, circuit)
          val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
          logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
          processModule(newPrefix, subModule, circuit)

        case DefNode(info, name, expression) =>
          logger.debug(s"declaration:DefNode:$name:${expression.serialize}")
          getAssigner(name, processExpression(expression))

        case DefWire(info, name, tpe) =>
          logger.debug(s"declaration:DefWire:$name")

        case DefRegister(info, name, tpe, clockExpression, resetExpression, initValueExpression) =>
          logger.debug(s"declaration:DefRegister:$name")
//          logger.debug(s"declaration:DefRegister:$name clock <- ${clockExpression.serialize} ${processExpression(clockExpression).serialize}")
//          logger.debug(s"declaration:DefRegister:$name reset <- ${resetExpression.serialize} ${processExpression(resetExpression).serialize}")
//          logger.debug(s"declaration:DefRegister:$name init  <- ${initValueExpression.serialize} ${processExpression(initValueExpression).serialize}")
          val expandedName = expand(name)

          val clockResult = processExpression(clockExpression)
          val resetResult = processExpression(resetExpression)
          val resetValue  = processExpression(initValueExpression)

          val registerIn  = symbolTable(s"$expandedName${ExpressionCompiler.RegisterInputSuffix}")
          val registerOut = symbolTable(expandedName)

          if(dataStore.numberOfBuffers > 1) {
            scheduler.scheduleCopy(registerIn)
          }

          registerIn.dataSize match {
            case IntSize =>
              triggeredAssign(clockResult, registerOut, dataStore.GetInt(registerIn.index))
              resetValue match {
                case rv: IntExpressionResult =>
                  triggeredAssign(resetResult, registerOut, rv)
                case rv: BigExpressionResult =>
                  triggeredAssign(resetResult, registerOut, ToInt(rv.apply))
              }
            case LongSize =>
//              scheduler.combinationalAssigns += dataStore.AssignLong(registerOut.index, dataStore.GetLong(registerIn.index).apply)
//              triggeredAssign(clockResult, registerOut, dataStore.GetLong(registerIn.index))
//              resetValue match {
//                case rv: IntExpressionResult => triggeredAssign(resetResult, registerOut, ToLong(rv.apply))
//                case rv: LongExpressionResult => triggeredAssign(resetResult, registerOut, rv)
//              }
              ???
            case BigSize =>
              scheduler.combinationalAssigns += dataStore.AssignBig(registerOut.index, dataStore.GetBig(registerIn.index).apply)
              triggeredAssign(clockResult, registerOut, dataStore.GetBig(registerIn.index))
              resetValue match {
                case rv: IntExpressionResult => triggeredAssign(resetResult, registerOut, ToBig(rv.apply))
                case rv: BigExpressionResult => triggeredAssign(resetResult, registerOut, rv)
              }
            case _ =>
              throw InterpreterException(s"bad register $statement")
          }
        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
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
          // logger.debug(s"got a conditionally $conditionally")
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
        val symbol = symbolTable(expand(port.name))
        if(dataStore.numberOfBuffers > 1) {
          scheduler.scheduleCopy(symbol)
        }
      }
    }

    myModule match {
      case module: firrtl.ir.Module =>
        processPorts(module)
        processStatements(module.body)
      case extModule: ExtModule => // Look to see if we have an implementation for this
        logger.debug(s"got external module ${extModule.name} instance $modulePrefix")
        processPorts(extModule)
        /* use exists while looking for the right factory, short circuits iteration when found */
//        logger.debug(s"Factories: ${dependencyGraph.blackBoxFactories.mkString("\n")}")
//        val implementationFound = dependencyGraph.blackBoxFactories.exists { factory =>
//          logger.debug("Found an existing factory")
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
  def compile(circuit: Circuit, blackBoxFactories: Seq[BlackBoxFactory]): Unit = {
    val module = FindModule(circuit.main, circuit) match {
      case regularModule: firrtl.ir.Module => regularModule
      case externalModule: firrtl.ir.ExtModule =>
        throw InterpreterException(s"Top level module must be a regular module $externalModule")
      case x =>
        throw InterpreterException(s"Top level module is not the right kind of module $x")
    }

    processModule("", module, circuit)
  }
}

object ExpressionCompiler {
  val RegisterInputSuffix = "/in"
}
