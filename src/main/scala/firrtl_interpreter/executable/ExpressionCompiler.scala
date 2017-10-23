// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.PrimOps._
import firrtl._
import firrtl.ir._
import firrtl_interpreter._

class ExpressionCompiler(program: Program, parent: FirrtlTerp) extends logger.LazyLogging {
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

        def handleIntResult(e1: IntExpressionResult, e2: IntExpressionResult): ExpressionResult = {
          opCode match {
            case Add => AddInts(e1.apply, e2.apply)
            case Sub => SubInts(e1.apply, e2.apply)
            case Mul => MulInts(e1.apply, e2.apply)
            case Div => DivInts(e1.apply, e2.apply)
            case Rem => RemInts(e1.apply, e2.apply)

            case Eq => EqInts(e1.apply, e2.apply)
            case Neq => NeqInts(e1.apply, e2.apply)
            case Lt => LtInts(e1.apply, e2.apply)
            case Leq => LeqInts(e1.apply, e2.apply)
            case Gt => GtInts(e1.apply, e2.apply)
            case Geq => GeqInts(e1.apply, e2.apply)

            case Dshl => DshlInts(e1.apply, e2.apply)
            case Dshr => DshrInts(e1.apply, e2.apply)

            case And => AndInts(e1.apply, e2.apply)
            case Or => OrInts(e1.apply, e2.apply)
            case Xor => XorInts(e1.apply, e2.apply)

            case Cat =>
              CatInts(e1.apply, arg1IsSigned, arg1Width, e2.apply, arg2IsSigned, arg2Width)

            case _ =>
              throw InterpreterException(s"Error:BinaryOp:$opCode)(${args.head}, ${args.tail.head})")
          }
        }

        def handleLongResult(e1: LongExpressionResult, e2: LongExpressionResult): ExpressionResult = {
          opCode match {
            case Add => AddLongs(e1.apply, e2.apply)
            case Sub => SubLongs(e1.apply, e2.apply)
            case Mul => MulLongs(e1.apply, e2.apply)
            case Div => DivLongs(e1.apply, e2.apply)
            case Rem => RemLongs(e1.apply, e2.apply)

            case Eq  => EqLongs(e1.apply, e2.apply)
            case Neq => NeqLongs(e1.apply, e2.apply)
            case Lt  => LtLongs(e1.apply, e2.apply)
            case Leq => LeqLongs(e1.apply, e2.apply)
            case Gt  => GtLongs(e1.apply, e2.apply)
            case Geq => GeqLongs(e1.apply, e2.apply)

            case Dshl => DshlLongs(e1.apply, e2.apply)
            case Dshr => DshrLongs(e1.apply, e2.apply)

            case And  => AndLongs(e1.apply, e2.apply)
            case Or   => OrLongs(e1.apply, e2.apply)
            case Xor  => XorLongs(e1.apply, e2.apply)

            case Cat =>
              CatLongs(e1.apply, arg1IsSigned, arg1Width, e2.apply, arg2IsSigned, arg2Width)

            case _ =>
              throw InterpreterException(s"Error:BinaryOp:$opCode(${args.head}, ${args.tail.head})")
          }
        }

        def handleBigResult(e1: BigExpressionResult, e2: BigExpressionResult): ExpressionResult = {
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
        }

        (arg1, arg2) match {
          case (e1: IntExpressionResult, e2: IntExpressionResult) =>
            handleIntResult(e1, e2)

          case (e1: IntExpressionResult, e2: LongExpressionResult) =>
            handleLongResult(ToLong(e1.apply), e2)

          case (e1: IntExpressionResult, e2: BigExpressionResult) =>
            handleBigResult(ToBig(e1.apply), e2)


          case (e1: LongExpressionResult, e2: IntExpressionResult) =>
            handleLongResult(e1, ToLong(e2.apply))

          case (e1: LongExpressionResult, e2: LongExpressionResult) =>
            handleLongResult(e1, e2)

          case (e1: LongExpressionResult, e2: BigExpressionResult) =>
            handleBigResult(LongToBig(e1.apply), e2)


          case (e1: BigExpressionResult, e2: IntExpressionResult) =>
            handleBigResult(e1, ToBig(e2.apply))

          case (e1: BigExpressionResult, e2: LongExpressionResult) =>
            handleBigResult(e1, LongToBig(e2.apply))

          case (e1: BigExpressionResult, e2: BigExpressionResult) =>
            handleBigResult(e1, e2)

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
          case e1: LongExpressionResult =>
            op match {
              case Head => HeadLongs(e1.apply, isSigned, takeBits = param1, arg1Width)
              case Tail => TailLongs(e1.apply, isSigned, toDrop = param1, arg1Width)
              case Shl  => ShlLongs(e1.apply, GetLongConstant(param1).apply)
              case Shr  => ShrLongs(e1.apply, GetLongConstant(param1).apply)
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
          case e1: LongExpressionResult =>
            op match {
              case Bits => BitsLongs(e1.apply, isSigned, arg2.toInt, arg3.toInt, width)
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
        val width = tpe match {
          case UIntType(IntWidth(n)) => n.toInt
          case SIntType(IntWidth(n)) => n.toInt
          case ClockType             => 1
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
          case e1: LongExpressionResult =>
            op match {
              case Pad     => e1
              case AsUInt  => e1
              case AsSInt  => e1
              case AsClock => e1

              case Cvt     => e1
              case Neg     => NegLongs(e1.apply)
              case Not     => NotLongs(e1.apply)

              case Andr    => AndrLongs(e1.apply, width)
              case Orr     => NotLongs(e1.apply)
              case Xorr    => NegLongs(e1.apply)
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
            case LongSize => dataStore.GetLong(symbol.index)
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
                  case (t: IntExpressionResult, f: LongExpressionResult) =>
                    MuxLongs(c.apply, ToLong(t.apply).apply, f.apply)
                  case (t: IntExpressionResult, f: BigExpressionResult) =>
                    MuxBigs(c.apply, ToBig(t.apply).apply, f.apply)

                  case (t: LongExpressionResult, f: IntExpressionResult) =>
                    MuxLongs(c.apply, t.apply, ToLong(f.apply).apply)
                  case (t: LongExpressionResult, f: LongExpressionResult) =>
                    MuxLongs(c.apply, t.apply, f.apply)
                  case (t: LongExpressionResult, f: BigExpressionResult) =>
                    MuxBigs(c.apply, LongToBig(t.apply).apply, f.apply)

                  case (t: BigExpressionResult, f: IntExpressionResult) =>
                    MuxBigs(c.apply, t.apply, ToBig(f.apply).apply)
                  case (t: BigExpressionResult, f: LongExpressionResult) =>
                    MuxBigs(c.apply, t.apply, LongToBig(f.apply).apply)
                  case (t: BigExpressionResult, f: BigExpressionResult) =>
                    MuxBigs(c.apply, t.apply, f.apply)
                }
              case c =>
                throw InterpreterException(s"Mux condition is not 1 bit $condition parsed as $c")
            }

          case WRef(name, _, _, _) =>
            createAccessor(expand(name))
          case subfield: WSubField =>
            createAccessor(expand(subfield.serialize))
          case subIndex: WSubIndex =>
            createAccessor(expand(subIndex.serialize))

          case ValidIf(condition, value, tpe) =>
            processExpression(condition) match {
              case c: IntExpressionResult =>
                processExpression(value) match {
                  case t: IntExpressionResult =>
                    MuxInts(c.apply, t.apply, UndefinedInts(getWidth(tpe)).apply)
                  case t: LongExpressionResult =>
                    MuxLongs(c.apply, t.apply, UndefinedLongs(getWidth(tpe)).apply)
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

      def getAssigner(symbol: Symbol, expressionResult: ExpressionResult): Assigner = {
        val assigner = (symbol.dataSize, expressionResult) match {
          case (IntSize,  result: IntExpressionResult)  => dataStore.AssignInt(symbol, result.apply)
          case (LongSize, result: IntExpressionResult)  => dataStore.AssignLong(symbol, ToLong(result.apply).apply)
          case (LongSize, result: LongExpressionResult) => dataStore.AssignLong(symbol, result.apply)
          case (BigSize,  result: IntExpressionResult)  => dataStore.AssignBig(symbol, ToBig(result.apply).apply)
          case (BigSize,  result: LongExpressionResult) => dataStore.AssignBig(symbol, LongToBig(result.apply).apply)
          case (BigSize,  result: BigExpressionResult)  => dataStore.AssignBig(symbol, result.apply)
          case (size, result) =>
            val expressionSize = result match {
              case _: IntExpressionResult  => "Int"
              case _: LongExpressionResult => "Long"
              case _: BigExpressionResult  => "Big"
            }

            throw InterpreterException(
              s"Error:assignment size mismatch ($size)${symbol.name} <= ($expressionSize)$expressionResult")
        }
        scheduler.combinationalAssigns += assigner
        assigner
      }

      def getAssignerByName(name: String, expressionResult: ExpressionResult): Assigner = {
        getAssigner(symbolTable(name), expressionResult)
      }

      def getIndirectAssigner(
                               symbol: Symbol,
                               indexSymbol: Symbol,
                               enableSymbol: Symbol,
                               expressionResult: ExpressionResult
                             ): Assigner = {

        val getIndex = dataStore.GetInt(indexSymbol.index).apply _
        val getEnable = dataStore.GetInt(enableSymbol.index).apply _

        val assigner = (symbol.dataSize, expressionResult) match {
          case (IntSize,  result: IntExpressionResult)  =>
            dataStore.AssignIntIndirect(symbol, getIndex, getEnable, result.apply)
          case (LongSize, result: IntExpressionResult)  =>
            dataStore.AssignLongIndirect(symbol, getIndex, getEnable, ToLong(result.apply).apply)
          case (LongSize, result: LongExpressionResult) =>
            dataStore.AssignLongIndirect(symbol, getIndex, getEnable, result.apply)
          case (BigSize,  result: IntExpressionResult)  =>
            dataStore.AssignBigIndirect(symbol, getIndex, getEnable, ToBig(result.apply).apply)
          case (BigSize,  result: LongExpressionResult) =>
            dataStore.AssignBigIndirect(symbol, getIndex, getEnable, LongToBig(result.apply).apply)
          case (BigSize,  result: BigExpressionResult)  =>
            dataStore.AssignBigIndirect(symbol, getIndex, getEnable, result.apply)
          case (size, result) =>
            val expressionSize = result match {
              case _: IntExpressionResult  => "Int"
              case _: LongExpressionResult => "Long"
              case _: BigExpressionResult  => "Big"
            }

            throw InterpreterException(
              s"Error:assignment size mismatch ($size)${symbol.name} <= ($expressionSize)$expressionResult")
        }
        scheduler.combinationalAssigns += assigner
        assigner
      }

      def makeGet(source: Symbol): ExpressionResult = {
        source.dataSize match {
          case IntSize =>
            dataStore.GetInt(source.index)
          case LongSize =>
            dataStore.GetLong(source.index)
          case BigSize =>
            dataStore.GetBig(source.index)
        }
      }

      def makeGetIndirect(memory: Symbol, data: Symbol, enable: Symbol, addr: Symbol): ExpressionResult = {
        data.dataSize match {
          case IntSize =>
            dataStore.GetIntIndirect(
              memory, dataStore.GetInt(addr.index).apply, dataStore.GetInt(enable.index).apply
            )
          case LongSize =>
            dataStore.GetLongIndirect(
              memory, dataStore.GetInt(addr.index).apply, dataStore.GetInt(enable.index).apply
            )
          case BigSize =>
            dataStore.GetBigIndirect(
              memory, dataStore.GetInt(addr.index).apply, dataStore.GetInt(enable.index).apply
            )
        }
      }

      //TODO (chick) This must be modified to be on the right clock
      //TODO (chick) Does mask need to be pipelined

      /**
        * Construct the machinery to move data into and out of the memory stack
        * @param memory
        * @param expandedName
        * @param scheduler
        */
      def buildMemoryInternals(memory: DefMemory, expandedName: String, scheduler: Scheduler): Unit = {
        val symbolTable = scheduler.symbolTable
        val memorySymbol = symbolTable(expandedName)
        val addrWidth = IntWidth(requiredBitsForUInt(memory.depth - 1))

        memory.readers.foreach { readerString =>
          val readerName = s"$expandedName.$readerString"
          val enable = symbolTable(s"$readerName.en")
          val clk    = symbolTable(s"$readerName.clk")
          val addr   = symbolTable(s"$readerName.addr")
          val data   = symbolTable(s"$readerName.data")

          val pipelineReadSymbols = (0 until memory.readLatency).map { n =>
            symbolTable(s"$expandedName.$readerString.pipeline_$n")
          } ++ Seq(data)

          getAssigner(pipelineReadSymbols.head, makeGetIndirect(memorySymbol, data, enable, addr))

          pipelineReadSymbols.zip(pipelineReadSymbols.tail).foreach { case (target, source) =>
            getAssigner(target, makeGet(source))
          }
        }

        memory.writers.foreach { writerString =>
          val writerName = s"$expandedName.$writerString"

          val enable = symbolTable(s"$writerName.en")
          val clk    = symbolTable(s"$writerName.clk")
          val addr   = symbolTable(s"$writerName.addr")
          val mask   = symbolTable(s"$writerName.mask")
          val data   = symbolTable(s"$writerName.data")

          val pipelineEnableSymbols = Seq(enable) ++ (0 until memory.writeLatency).map { n =>
            symbolTable(s"$expandedName.$writerString.pipeline_en_$n")
          }
          val pipelineDataSymbols = Seq(data) ++ (0 until memory.writeLatency).map { n =>
            symbolTable(s"$expandedName.$writerString.pipeline_data_$n")
          }
          val pipelineAddrSymbols = Seq(addr) ++ (0 until memory.writeLatency).map { n =>
            symbolTable(s"$expandedName.$writerString.pipeline_addr_$n")
          }

          def connectPipeline(pipeline: Seq[Symbol]): Unit = {
            pipeline.zip(pipeline.tail).foreach { case (source, target) =>
              getAssigner(target, makeGet(source))
            }
          }

          connectPipeline(pipelineEnableSymbols)
          connectPipeline(pipelineDataSymbols)
          connectPipeline(pipelineAddrSymbols)

          getIndirectAssigner(
            memorySymbol,
            pipelineAddrSymbols.last,
            pipelineEnableSymbols.last,
            makeGet(pipelineDataSymbols.last)
          )
        }

        memory.readwriters.foreach { readWriterString =>
          val writerName = s"$expandedName.$readWriterString"

          val enable =  symbolTable(s"$writerName.en")
          val clk    =  symbolTable(s"$writerName.clk")
          val addr   =  symbolTable(s"$writerName.addr")
          val rdata  =  symbolTable(s"$writerName.rdata")
          val mode   =  symbolTable(s"$writerName.wmode")
          val mask   =  symbolTable(s"$writerName.wmask")
          val wdata  =  symbolTable(s"$writerName.wdata")

          val pipelineReadSymbols = (0 until memory.readLatency).map { n =>
            symbolTable(s"$expandedName.$readWriterString.pipeline_read_$n")
          } ++ Seq(rdata)

          getAssigner(pipelineReadSymbols.head, makeGetIndirect(memorySymbol, rdata, enable, addr))

          pipelineReadSymbols.zip(pipelineReadSymbols.tail).foreach { case (target, source) =>
            getAssigner(target, makeGet(source))
          }

          val pipelineEnableSymbols = Seq(enable) ++ (0 until memory.writeLatency).map { n =>
            symbolTable(s"$expandedName.$readWriterString.pipeline_en_$n")
          }
          val pipelineDataSymbols = Seq(wdata) ++ (0 until memory.writeLatency).map { n =>
            symbolTable(s"$expandedName.$readWriterString.pipeline_write_data_$n")
          }
          val pipelineAddrSymbols = Seq(addr) ++ (0 until memory.writeLatency).map { n =>
            symbolTable(s"$expandedName.$readWriterString.pipeline_write_addr_$n")
          }

          def connectPipeline(pipeline: Seq[Symbol]): Unit = {
            pipeline.zip(pipeline.tail).foreach { case (source, target) =>
              getAssigner(target, makeGet(source))
            }
          }

          connectPipeline(pipelineEnableSymbols)
          connectPipeline(pipelineDataSymbols)
          connectPipeline(pipelineAddrSymbols)

          getIndirectAssigner(
            memorySymbol,
            pipelineAddrSymbols.last,
            pipelineEnableSymbols.last,
            makeGet(pipelineDataSymbols.last)
          )
        }
      }

      def triggeredAssign(
                           triggerExpression: ExpressionResult,
                           value: Symbol,
                           expressionResult: ExpressionResult
                         ): Unit = {
        val assignment = (value.dataSize, expressionResult) match {
          case (IntSize, e: IntExpressionResult) => dataStore.AssignInt(value, e.apply)
          case (LongSize, e: LongExpressionResult) => dataStore.AssignLong(value, e.apply)
          case (BigSize, e: BigExpressionResult) => dataStore.AssignBig(value, e.apply)
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
          val lhsName = expand(renameIfRegister(con.loc.serialize))
          getAssignerByName(lhsName, processExpression(con.expr))

        case WDefInstance(info, instanceName, moduleName, _) =>
          val subModule = FindModule(moduleName, circuit)
          val newPrefix = if(modulePrefix.isEmpty) instanceName else modulePrefix + "." + instanceName
          logger.debug(s"declaration:WDefInstance:$instanceName:$moduleName prefix now $newPrefix")
          processModule(newPrefix, subModule, circuit)

        case DefNode(info, name, expression) =>
          logger.debug(s"declaration:DefNode:$name:${expression.serialize}")
          getAssignerByName(name, processExpression(expression))

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

          registerIn.dataSize match {
            case IntSize =>
              triggeredAssign(clockResult, registerOut, dataStore.GetInt(registerIn.index))
              resetValue match {
                case rv: IntExpressionResult =>
                  triggeredAssign(resetResult, registerOut, rv)
                case rv: LongExpressionResult =>
                  triggeredAssign(resetResult, registerOut, LongToInt(rv.apply))
                case rv: BigExpressionResult =>
                  triggeredAssign(resetResult, registerOut, ToInt(rv.apply))
              }
            case LongSize =>
              scheduler.combinationalAssigns += dataStore.AssignLong(registerOut, dataStore.GetLong(registerIn.index).apply)
              triggeredAssign(clockResult, registerOut, dataStore.GetLong(registerIn.index))
              resetValue match {
                case rv: IntExpressionResult => triggeredAssign(resetResult, registerOut, ToLong(rv.apply))
                case rv: LongExpressionResult => triggeredAssign(resetResult, registerOut, rv)
                case rv: BigExpressionResult => triggeredAssign(resetResult, registerOut, BigToLong(rv.apply))
              }
            case BigSize =>
              scheduler.combinationalAssigns += dataStore.AssignBig(registerOut, dataStore.GetBig(registerIn.index).apply)
              triggeredAssign(clockResult, registerOut, dataStore.GetBig(registerIn.index))
              resetValue match {
                case rv: IntExpressionResult => triggeredAssign(resetResult, registerOut, ToBig(rv.apply))
                case rv: LongExpressionResult => triggeredAssign(resetResult, registerOut, LongToBig(rv.apply))
                case rv: BigExpressionResult => triggeredAssign(resetResult, registerOut, rv)
              }
            case _ =>
              throw InterpreterException(s"bad register $statement")
          }
        case defMemory: DefMemory =>
          val expandedName = expand(defMemory.name)
          logger.debug(s"declaration:DefMemory:${defMemory.name} becomes $expandedName")
          buildMemoryInternals(defMemory, expandedName, scheduler)
        case IsInvalid(info, expression) =>
//          IsInvalid(info, processExpression(expression))
        case Stop(info, ret, clockExpression, enableExpression) =>
          val stopOp = StopOp(info, returnValue = ret, condition = processExpression(enableExpression), parent)
          scheduler.triggeredAssigns(processExpression(clockExpression)) += stopOp

        case Print(info, stringLiteral, argExpressions, clockExpression, enableExpression) =>
          val printfOp = PrintfOp(
            info, stringLiteral,
            argExpressions.map { expression => processExpression(expression) },
            processExpression(enableExpression)
          )
          scheduler.triggeredAssigns(processExpression(clockExpression)) += printfOp

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

    def processPorts(module: DefModule): Unit = {
      for(port <- module.ports) {
        val symbol = symbolTable(expand(port.name))
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
