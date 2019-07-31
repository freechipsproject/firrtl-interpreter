// See LICENSE for license details.

package firrtl_interpreter

import firrtl._
import firrtl.ir._
import firrtl.PrimOps._

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

  var defaultKeysToResolve: Array[String] = {
    val keys = new mutable.HashSet[String]

    keys ++= circuitState.memories.flatMap {
      case (_, memory) => memory.getAllFieldDependencies
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

    /*
    Check to see if this output found on the rhs of dependency relationship
    has been computed yet
     */
    if(circuitState.isOutput(key)) {
      if(! circuitState.rhsOutputs.contains(key) &&
         dependencyGraph.nameToExpression.contains(key)) {
        resolveDependency(key)
        circuitState.rhsOutputs += key
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

  def makeUIntValue(value: BigInt, intWidth: IntWidth): UIntLiteral = {
    val maskedValue = mask(value, intWidth.width)
    UIntLiteral(maskedValue, intWidth)
  }

  def makeSIntValue(value: BigInt, intWidth: IntWidth): SIntLiteral = {
    val maskedValue = mask(value, intWidth.width)
    SIntLiteral(maskedValue, intWidth)
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
      case Add => arg1 + arg2
      case Sub => arg1 - arg2
      case Mul => arg1 * arg2
      case Div => arg1 / arg2
      case Rem => arg1 % arg2
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
      case Eq      => arg1 == arg2
      case Neq     => arg1 != arg2
      case Lt       => arg1 <  arg2
      case Leq    => arg1 <= arg2
      case Gt    => arg1 >  arg2
      case Geq => arg1 >= arg2
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
      case AsUInt  => e.asUInt
      case AsSInt  => e.asSInt
      case AsClock => e.asClock
    }
  }

  def bitOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)
    val n = parameters.head

    opCode match {
      case Shl => e << n
      case Shr => e >> n
      case Head => e.head(n)
      case Tail => e.tail(n)
    }
  }
  def dynamicBitOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)
    val n = evaluate(args.tail.head)

    opCode match {
      case Dshl => e << n
      case Dshr => e >> n
    }
  }
  def oneArgOps(opCode: PrimOp, args: Seq[Expression], parameters: Seq[BigInt], tpe: Type): Concrete = {
    val e = evaluate(args.head)

    opCode match {
      case Cvt         => e.cvt
      case Neg             => e.neg
      case Not             => e.not
      case Andr      => e.andReduce
      case Orr       => e.orReduce
      case Xorr      => e.xorReduce
    }
  }
  def binaryBitWise(opCode: PrimOp, args: Seq[Expression], tpe: Type): Concrete = {
    val arg1 = evaluate(args.head)
    val arg2 = evaluate(args.tail.head)
    opCode match {
      case And    => arg1 & arg2
      case Or     => arg1 | arg2
      case Xor    => arg1 ^ arg2
      case Cat => arg1.cat(arg2)
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
            case ConcreteUInt(value, 1, _) =>
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
        case WRef(name, tpe, _, _) => getValue(name).forceWidth(tpe)
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
              case ClockType             => Concrete.randomClock()
              case other =>
                throw InterpreterException(s"ValidIf found unsupported type: $other")
            }
          }
        case DoPrim(op, args, const, tpe) =>
          val v = op match {
            case Add => mathPrimitive(op, args, tpe)
            case Sub => mathPrimitive(op, args, tpe)
            case Mul => mathPrimitive(op, args, tpe)
            case Div => mathPrimitive(op, args, tpe)
            case Rem => mathPrimitive(op, args, tpe)

            case Eq => comparisonOp(op, args, tpe)
            case Neq => comparisonOp(op, args, tpe)
            case Lt => comparisonOp(op, args, tpe)
            case Leq => comparisonOp(op, args, tpe)
            case Gt => comparisonOp(op, args, tpe)
            case Geq => comparisonOp(op, args, tpe)

            case Pad => paddingOp(op, args, const, tpe)

            case AsUInt => castingOp(op, args, tpe)
            case AsSInt => castingOp(op, args, tpe)
            case AsClock => castingOp(op, args, tpe)

            case Shl => bitOps(op, args, const, tpe)
            case Shr => bitOps(op, args, const, tpe)

            case Dshl => dynamicBitOps(op, args, const, tpe)
            case Dshr => dynamicBitOps(op, args, const, tpe)

            case Cvt => oneArgOps(op, args, const, tpe)
            case Neg => oneArgOps(op, args, const, tpe)
            case Not => oneArgOps(op, args, const, tpe)

            case And => binaryBitWise(op, args, tpe)
            case Or => binaryBitWise(op, args, tpe)
            case Xor => binaryBitWise(op, args, tpe)

            case Andr => oneArgOps(op, args, const, tpe)
            case Orr => oneArgOps(op, args, const, tpe)
            case Xorr => oneArgOps(op, args, const, tpe)

            case Cat => binaryBitWise(op, args, tpe)

            case Bits => bitSelectOp(op, args, const, tpe)

            case Head => bitOps(op, args, const, tpe)
            case Tail => bitOps(op, args, const, tpe)

            case _ =>
              throw new InterruptedException(s"PrimOP $op in $expression not yet supported $sourceInfo")
          }
          v.forceWidth(tpe)
        case c: UIntLiteral => Concrete(c).forceWidth(c.tpe)
        case c: SIntLiteral => Concrete(c).forceWidth(c.tpe)
        case blackBoxOutput: BlackBoxOutput =>
          log(s"got a black box, $blackBoxOutput")
          val concreteInputs = blackBoxOutput.dependentInputs.map { input => getValue(input)}
          blackBoxOutput.execute(concreteInputs)
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
        case Some(_) =>
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

  /**
    * when resolving registers dependency, consider the resetCondition
    * to be the dependency if appropriate
    * @param key  name of register
    * @return     new concrete value for register
    */
  def resolveRegister(key: String): Concrete = {
    val registerDef = dependencyGraph.registers(key)
    val resetCondition = evaluate(registerDef.reset)
    val newValue = if(resetCondition.value > 0 ) {
      val resetValue = {
        evaluate(registerDef.init).forceWidth(typeToWidth(dependencyGraph.nameToType(registerDef.name)))
      }
      resetValue
    }
    else {
      val expression = dependencyGraph.nameToExpression(key)
      evaluate(expression, Some(key))
    }
    newValue
  }
  private def resolveDependency(key: String): Concrete = {
    resolveDepth += 1

    val value = timer(key) {
      if (circuitState.isInput(key)) {
        circuitState.getValue(key).get
      }
      else if(circuitState.isRegister(key)) {
        resolveRegister(key)
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
        //      Concrete.poisonedUInt(1)
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
      if(specificDependencies.nonEmpty) {
        specificDependencies
      }
      else if(useTopologicalSortedKeys && keyOrderInitialized) {
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
      if(verbose) {
        println(s"Key order ${orderedKeysToResolve.mkString("\n")}")
      }
      keyOrderInitialized = true
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

  def executeFormattedPrint(formatString: String, allArgs: Seq[Any]): String = {
    val outBuffer = new StringBuilder
    var s = formatString
    var args = allArgs

    while(s.nonEmpty) {
      s.indexOf("%") match {
        case -1 =>
          outBuffer ++= s
          s = ""
        case offset =>
          outBuffer ++= s.take(offset)
          s = s.drop(offset + 1)
          s.headOption match {
            case Some('%') =>
              outBuffer ++= "%"
              s = s.tail
            case Some('b') =>
              outBuffer ++= BigInt(args.head.toString).toString(2)
              args = args.tail
              s = s.tail
            case Some('c') =>
              outBuffer += BigInt(args.head.toString).toChar
              args = args.tail
              s = s.tail
            case Some(specifier)   =>
              //noinspection ScalaUnnecessaryParentheses
              outBuffer ++= (s"%$specifier").format(BigInt(args.head.toString))
              args = args.tail
              s = s.tail
            case _ =>
              s = ""
          }
      }
    }
    StringContext.treatEscapes(outBuffer.toString())
  }

  def checkPrints(): Unit = {
    for(printStatement <- dependencyGraph.prints) {
      val condition = evaluate(printStatement.en)
      if(condition.value > 0) {
        val resolvedArgs = printStatement.args.map { arg =>
          evaluate(arg).value
        }
        val formatString = printStatement.string.serialize
        print(executeFormattedPrint(formatString, resolvedArgs))
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
