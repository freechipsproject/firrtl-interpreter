// See LICENSE for license details.

package firrtl_interpreter

import firrtl._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * This is the evaluation engine for the FirrtlTerp
  * it requires the previousState of the system
  *
  * @param circuitState  the state of the system, should not be modified before all dependencies have been resolved
  */
class LoFirrtlExpressionEvaluator(val dependencyGraph: DependencyGraph, val circuitState: CircuitState)
  extends SimpleLogger {
  var toResolve = mutable.HashSet(dependencyGraph.keys.toSeq:_*)

  var evaluateAll = false

  var exceptionCaught = false

  var useTopologicalSortedKeys = false

  var allowCombinationalLoops = false

  val evaluationStack = new ExpressionExecutionStack(this)

  var defaultKeysToResolve = {
    val keys = new mutable.HashSet[String]

    keys ++= circuitState.memories.flatMap {
      case (name, memory) => memory.getAllFieldDependencies
    }.filter(dependencyGraph.nameToExpression.contains)

    keys ++= dependencyGraph.outputPorts

    keys ++= dependencyGraph.registerNames
    keys.toArray
  }

  var keyOrderInitialized = false
  val orderedKeysToResolve = new ArrayBuffer[String]()

  val timer = new Timer
  timer.enabled = false

  /**
    * get the value from the current circuit state, if it is dependent on something else
    * we haven't computed yet. resolve this new dependency first then pull it's value from the
    * current state
    *
    * @param key  the name of the assignable thing
    * @return
    */
  def getValue(key: String): Concrete = {
    if(dependencyGraph.memoryOutputKeys.contains(key)) {
      val dependentKeys = dependencyGraph.memoryOutputKeys(key)
      for (elem <- dependentKeys) {
        resolveDependency(elem)
      }
    }
    circuitState.getValue(key) match {
      case Some(value) => value
      case _ =>
        resolveDependency(key)
    }
  }

  /**
    * mask off bits above size in a BigInt,
    * uses modulo, constructing the modulo base on the size
    * working around BigInt's shift by int requirement
    *
    * @param number number to mask
    * @param size   how many bits to keep
    * @return
    */
  def mask(number: BigInt, size: BigInt): BigInt = {
    if(size < 1) return Big0
    val convenientShiftSize = 30
    var modulo: BigInt = 1
    var toShift: BigInt = (size - 1).max(0) + 1
    while(toShift > 0) {
      modulo = modulo << toShift.min(convenientShiftSize).toInt
      toShift -= convenientShiftSize
    }
    number % modulo
  }

  /**
    * shifts number right
    *
    * @param number number to shift
    * @param size how many bits to shift
    * @return
    */
  def shiftRight(number: BigInt, size: BigInt): BigInt = {
    val convenientShiftSize = 30
    var toShift: BigInt = size.max(0)
    var shiftedNumber: BigInt = number
    while(toShift > 0) {
      shiftedNumber = shiftedNumber >> toShift.min(convenientShiftSize).toInt
      toShift -= convenientShiftSize
    }
    shiftedNumber
  }
  /**
    * shifts number left
    *
    * @param number number to shift
    * @param size how many bits to shift
    * @return
    */
  def shiftLeft(number: BigInt, size: BigInt): BigInt = {
    val convenientShiftSize = 30
    var toShift: BigInt = size.max(0)
    var shiftedNumber: BigInt = number
    while(toShift > 0) {
      shiftedNumber = shiftedNumber << toShift.min(convenientShiftSize).toInt
      toShift -= convenientShiftSize
    }
    shiftedNumber
  }

  def makeUIntValue(value: BigInt, intWidth: IntWidth): UIntValue = {
    val maskedValue = mask(value, intWidth.width)
    UIntValue(maskedValue, intWidth)
  }

  def makeSIntValue(value: BigInt, intWidth: IntWidth): SIntValue = {
    val maskedValue = mask(value, intWidth.width)
    SIntValue(maskedValue, intWidth)
  }

  def getWidth(tpe: Type): IntWidth = {
    val intWidth: IntWidth = tpe match {
      case UIntType(width: IntWidth) => width
      case SIntType(width: IntWidth) => width
    }
    intWidth
  }

  def mathPrimitive(opCode: PrimOp, args: Seq[Expression], tpe: Type): Concrete = {
    val arg1 = evaluate(args.head)
    val arg2 = evaluate(args.tail.head)
    opCode match {
      case ADD_OP => arg1 + arg2
      case SUB_OP => arg1 - arg2
      case MUL_OP => arg1 * arg2
      case DIV_OP => arg1 / arg2
      case REM_OP => arg1 % arg2
    }
  }

  def bitSelectOp(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)
    val hi = parameters.head
    val lo = parameters.tail.head
    e.bits(hi, lo)
  }

  def comparisonOp(opCode: PrimOp, args: Seq[Expression], tpe: Type): Concrete = {
    val arg1 = evaluate(args.head)
    val arg2 = evaluate(args.tail.head)
    opCode match {
      case EQUAL_OP      => arg1 == arg2
      case NEQUAL_OP     => arg1 != arg2
      case LESS_OP       => arg1 <  arg2
      case LESS_EQ_OP    => arg1 <= arg2
      case GREATER_OP    => arg1 >  arg2
      case GREATER_EQ_OP => arg1 >= arg2
    }
  }

  def paddingOp(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)
    val n = parameters.head

    e.pad(n)
  }

  def castingOp(opCode: PrimOp, args: Seq[Expression], tpe: Type): Concrete = {
    val e = evaluate(args.head)

    opCode match {
      case AS_UINT_OP  => e.asUInt
      case AS_SINT_OP  => e.asSInt
      case AS_CLOCK_OP => e.asClock
    }
  }

  def bitOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)
    val n = parameters.head

    opCode match {
      case SHIFT_LEFT_OP => e << n
      case SHIFT_RIGHT_OP => e >> n
      case HEAD_OP => e.head(n)
      case TAIL_OP => e.tail(n)
    }
  }
  def dynamicBitOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)
    val n = evaluate(args.tail.head)

    opCode match {
      case DYN_SHIFT_LEFT_OP => e << n
      case DYN_SHIFT_RIGHT_OP => e >> n
    }
  }
  def oneArgOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)

    opCode match {
      case CONVERT_OP         => e.cvt
      case NEG_OP             => e.neg
      case NOT_OP             => e.not
      case AND_REDUCE_OP      => e.andReduce
      case OR_REDUCE_OP       => e.orReduce
      case XOR_REDUCE_OP      => e.xorReduce
    }
  }
  def binaryBitWise(opCode: PrimOp, args: Seq[Expression], tpe: Type): Concrete = {
    val arg1 = evaluate(args.head)
    val arg2 = evaluate(args.tail.head)
    opCode match {
      case AND_OP    => arg1 & arg2
      case OR_OP     => arg1 | arg2
      case XOR_OP    => arg1 ^ arg2
      case CONCAT_OP => arg1.cat(arg2)
    }
  }
  /**
    * evaluate expression, if this expression references an ephemeral value (wire or node) that has
    * not been evaluated yet, recursively evaluate that reference first.  LoFirrtl guarantees that
    * there will be no loops here
    *
    * @param expression a LoFirrtl expression to evaluate
    * @return the resulting Concrete
    *
    * Note: OpCodes here are double matched, once in main loop herein, then again in function suitable for that
    * family of opCodes, it makes the code cleaner, I think, but may ultimately need to be inlined for performance
    */
  // scalastyle:off cyclomatic.complexity method.length
  def evaluate(expression: Expression, leftHandSideOption: Option[String] = None): Concrete = {
    log(
      leftHandSideOption match {
        case Some(key) =>
          s"evaluate     ${leftHandSideOption.getOrElse("")} <= ${expression.serialize} ${dependencyGraph.getInfo(key)}"
        case _         =>
          s"evaluate     ${expression.serialize}"
      }
    )
    indent()

    if(! evaluationStack.push(leftHandSideOption, expression)) {
      if(allowCombinationalLoops) {
        log(s"Combinational loop detected, second evaluation of ${leftHandSideOption.getOrElse("")}, returning 1.U")
        return ConcreteUInt(1, 1)
      }
    }

    def sourceInfo: String = {
      leftHandSideOption match {
        case Some(key) => dependencyGraph.getInfo(key)
        case _ => ""
      }
    }

    val result = try {
      expression match {
        case Mux(condition, trueExpression, falseExpression, tpe) =>
          evaluate(condition) match {
            case ConcreteUInt(value, 1) =>
              val v = if (value > 0) {
                if(evaluateAll) { evaluate(falseExpression)}
                evaluate(trueExpression)
              }
              else {
                if(evaluateAll) { evaluate(trueExpression)}
                evaluate(falseExpression)
              }
              v.forceWidth(tpe)
            case v =>
              throw InterpreterException(s"mux($condition) must be (0|1).U<1> was $v $sourceInfo")
          }
        case WRef(name, tpe, kind, gender) => getValue(name).forceWidth(tpe)
        case ws: WSubField =>
          val name = ws.serialize
          getValue(name).forceWidth(ws.tpe)
        case ws: WSubIndex =>
          val name = ws.serialize
          getValue(name).forceWidth(ws.tpe)
        case ValidIf(condition, value, tpe) =>
          if (evaluate(condition).value > 0) {
            evaluate(value).forceWidth(tpe)
          }
          else {
            if(evaluateAll) { evaluate(value)}

            tpe match {
              case UIntType(IntWidth(w)) => Concrete.randomUInt(w.toInt)
              case SIntType(IntWidth(w)) => Concrete.randomSInt(w.toInt)
              case ClockType()           => Concrete.randomClock()
            }
          }
        case DoPrim(op, args, const, tpe) =>
          val v = op match {
            case ADD_OP => mathPrimitive(op, args, tpe)
            case SUB_OP => mathPrimitive(op, args, tpe)
            case MUL_OP => mathPrimitive(op, args, tpe)
            case DIV_OP => mathPrimitive(op, args, tpe)
            case REM_OP => mathPrimitive(op, args, tpe)

            case EQUAL_OP => comparisonOp(op, args, tpe)
            case NEQUAL_OP => comparisonOp(op, args, tpe)
            case LESS_OP => comparisonOp(op, args, tpe)
            case LESS_EQ_OP => comparisonOp(op, args, tpe)
            case GREATER_OP => comparisonOp(op, args, tpe)
            case GREATER_EQ_OP => comparisonOp(op, args, tpe)

            case PAD_OP => paddingOp(op, args, const, tpe)

            case AS_UINT_OP => castingOp(op, args, tpe)
            case AS_SINT_OP => castingOp(op, args, tpe)
            case AS_CLOCK_OP => castingOp(op, args, tpe)

            case SHIFT_LEFT_OP => bitOps(op, args, const, tpe)
            case SHIFT_RIGHT_OP => bitOps(op, args, const, tpe)

            case DYN_SHIFT_LEFT_OP => dynamicBitOps(op, args, const, tpe)
            case DYN_SHIFT_RIGHT_OP => dynamicBitOps(op, args, const, tpe)

            case CONVERT_OP => oneArgOps(op, args, const, tpe)
            case NEG_OP => oneArgOps(op, args, const, tpe)
            case NOT_OP => oneArgOps(op, args, const, tpe)

            case AND_OP => binaryBitWise(op, args, tpe)
            case OR_OP => binaryBitWise(op, args, tpe)
            case XOR_OP => binaryBitWise(op, args, tpe)

            case AND_REDUCE_OP => oneArgOps(op, args, const, tpe)
            case OR_REDUCE_OP => oneArgOps(op, args, const, tpe)
            case XOR_REDUCE_OP => oneArgOps(op, args, const, tpe)

            case CONCAT_OP => binaryBitWise(op, args, tpe)

            case BITS_SELECT_OP => bitSelectOp(op, args, const, tpe)

            case HEAD_OP => bitOps(op, args, const, tpe)
            case TAIL_OP => bitOps(op, args, const, tpe)

            case _ =>
              throw new InterruptedException(s"PrimOP $op in $expression not yet supported $sourceInfo")
          }
          v.forceWidth(tpe)
        case c: UIntValue => Concrete(c).forceWidth(c.tpe)
        case c: SIntValue => Concrete(c).forceWidth(c.tpe)
      }
    }
    catch {
      case ie: Exception =>
        if(! exceptionCaught) {
          println(s"Exception during evaluation: ${ie.getMessage} $sourceInfo")
          showStack()
          exceptionCaught = true
        }
        throw ie
      case ie: AssertionError =>
        if(! exceptionCaught) {
          println(s"Assertion during evaluation: ${ie.getMessage} $sourceInfo")
          showStack()
          exceptionCaught = true
        }
        throw ie
    }

    evaluationStack.pop()
    dedent()
    log(
      leftHandSideOption match {
        case Some(key) =>
          s"evaluated    ${leftHandSideOption.getOrElse("")} <= $result"
        case _         =>
          s"evaluated     $result"
      }
    )

    result
  }
  // scalastyle:on


  def showStack(): Unit = {
    println("Expression Evaluation stack")
    println(evaluationStack.stackListing)
  }

  private def resolveDependency(key: String): Concrete = {
    resolveDepth += 1

    val value = timer(key) {
      if (circuitState.isInput(key)) {
        circuitState.getValue(key).get
      }
      else if (dependencyGraph.nameToExpression.contains(key)) {
        val expression = dependencyGraph.nameToExpression(key)
        evaluate(expression, Some(key))
      }
      else if (dependencyGraph.memoryKeys.contains(key)) {
        dependencyGraph.memoryKeys(key).getValue(key)
      }
      else {
        throw new InterpreterException(s"error: don't know what to do with key $key")
        //      ConcreteUInt(0, 1)
      }
    }

    if(useTopologicalSortedKeys && ! keyOrderInitialized) {
      orderedKeysToResolve += key
    }
    circuitState.setValue(key, value)

    resolveDepth -= 1

    value
  }

  def resolveDependencies(specificDependencies: Iterable[String]): Unit = {
    val toResolve: Iterable[String] = {
      if(useTopologicalSortedKeys && keyOrderInitialized) {
        orderedKeysToResolve
      } else {
        defaultKeysToResolve
      }
    }

    exceptionCaught = false
    evaluationStack.clear()

    for(key <- toResolve) {
      resolveDependency(key)
    }

    if(useTopologicalSortedKeys && ! keyOrderInitialized) {
      println(s"Key order ${orderedKeysToResolve.mkString("\n")}")
      keyOrderInitialized = true
    }
  }

  def processRegisterResets(): Unit = {
    for(registerDef <- dependencyGraph.registers) {
      val resetCondition = evaluate(registerDef.reset)
      if(resetCondition.value > 0 ) {
        val resetValue = {
          evaluate(registerDef.init).forceWidth(typeToWidth(dependencyGraph.nameToType(registerDef.name)))
        }
        // println(s"Register ${registerDef.name} reset to $resetValue")
        circuitState.nextRegisters(registerDef.name) = resetValue
      }
    }
  }

  def checkStops(): Option[Int] = {
    for(stopStatement <- dependencyGraph.stops) {
      if(evaluate(stopStatement.en, Some("stop")).value > 0) {
        if(stopStatement.ret == 0) {
          println(s"Success:${stopStatement.info}")
          return Some(0)
        }
        else {
          println(s"Failure:${stopStatement.info} returned ${stopStatement.ret}")
          return Some(stopStatement.ret)
        }
      }
    }
    None
  }

  def checkPrints(): Unit = {
    for(printStatement <- dependencyGraph.prints) {
      val condition = evaluate(printStatement.en)
      if(condition.value > 0) {
        val resolvedArgs = printStatement.args.map { case arg =>
          evaluate(arg).value
        }
        val formatString = printStatement.string.array.map(_.toChar).mkString("")
        printf(formatString, resolvedArgs:_*)
      }
    }
  }
  private var resolveDepth = 0
  private def indent(): Unit = resolveDepth += 1
  private def dedent(): Unit = resolveDepth -= 1
  override def log(message: => String): Unit = {
    if(verbose) {
      println(s"${" "*(resolveDepth*2)}$message")
    }
  }
}
