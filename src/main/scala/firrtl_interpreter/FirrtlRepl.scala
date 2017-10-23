// See LICENSE for license details.
package firrtl_interpreter

import java.io.File
import java.util.regex.Matcher

import firrtl_interpreter.vcd.VCD

import scala.collection.mutable.ArrayBuffer
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.{Terminal, TerminalFactory}
import scala.tools.jline.console.completer._
import collection.JavaConverters._
import scala.util.matching.Regex

abstract class Command(val name: String) {
  def run(args: Array[String])
  def usage: (String, String)
  def completer: Option[ArgumentCompleter] = {
    Some(new ArgumentCompleter(
      new StringsCompleter({name})
    ))
  }
}

class FirrtlRepl(val optionsManager: InterpreterOptionsManager with HasReplConfig) {
  val replConfig: ReplConfig = optionsManager.replConfig
  val interpreterOptions: InterpreterOptions = optionsManager.interpreterOptions

  firrtl_interpreter.random.setSeed(interpreterOptions.randomSeed)

  val terminal: Terminal = TerminalFactory.create()
  val console = new ConsoleReader
  private val historyPath = "~/.firrtl_repl_history".replaceFirst("^~",Matcher.quoteReplacement(System.getProperty("user.home")))
  val historyFile = new File(historyPath)
  if(! historyFile.exists()) {
    println(s"creating ${historyFile.getName}")
    historyFile.createNewFile()
  }
  val history = new FileHistory(historyFile)

  history.load(historyFile)
  console.setHistory(history)

  var currentInterpreterOpt: Option[FirrtlTerp] = None

  def interpreter: FirrtlTerp = currentInterpreterOpt.get
  var args = Array.empty[String]
  var done = false

  var inScript = false
  val scriptFactory = ScriptFactory(this)
  var currentScript: Option[Script] = None
  val IntPattern: Regex = """(-?\d+)""".r

  var currentVcdScript: Option[VCD] = None
  var replVcdController: Option[ReplVcdController] = None

  def loadSource(input: String): Unit = {
    currentInterpreterOpt = Some(FirrtlTerp(input, optionsManager))
    currentInterpreterOpt.foreach { _=>
      interpreter.evaluator.allowCombinationalLoops = interpreterOptions.allowCycles
      interpreter.evaluator.useTopologicalSortedKeys = interpreterOptions.setOrderedExec
      interpreter.evaluator.evaluationStack.maxExecutionDepth = interpreterOptions.maxExecutionDepth
      interpreter.setVerbose(interpreterOptions.setVerbose)

      console.println(s"Flags: $showFlags")
      console.println(
        s"dependency graph ${interpreter.dependencyGraph.validNames.size} " +
          s"elements ${interpreter.dependencyGraph.numberOfStatements} " +
          s"statements ${interpreter.dependencyGraph.numberOfNodes} nodes"
      )
      interpreter.evaluator.timer.enabled = true
    }
    buildCompletions()
  }

  def loadFile(fileName: String): Unit = {
    var file = new File(fileName)
    if(! file.exists()) {
      file = new File(fileName + ".fir")
      if(! file.exists()) {
        throw new Exception(s"file $fileName does not exist")
      }
    }
    val input = io.Source.fromFile(file).mkString
    console.println(s"loaded firrtl file $fileName")
    loadSource(input)
  }

  def loadScript(fileName: String): Unit = {
    currentScript = scriptFactory(fileName)
    currentScript match {
      case Some(script) =>
        console.println(s"loaded script file ${script.fileName} with ${script.length} lines")
      case _ =>
    }
  }

  def loadVcdScript(fileName: String): Unit = {
    val dutName = currentInterpreterOpt match {
      case Some(interpreter) => interpreter.ast.main
      case None => ""
    }
    try {
      currentVcdScript = Some(VCD.read(fileName, dutName))
      replVcdController = Some(new ReplVcdController(this, this.interpreter, currentVcdScript.get))
      console.println(s"loaded vcd script file $fileName\n${replVcdController.get.vcd.info}")
    }
    catch {
      case e: Exception =>
        console.println(s"Failed to load vcd script $fileName, error: ${e.getMessage}")
    }
  }

  def parseNumber(numberString: String): BigInt = {
    def parseWithRadix(numString: String, radix: Int): BigInt = {
      BigInt(numString, radix)
    }

    if(numberString.startsWith("0x"))     { parseWithRadix(numberString.drop(2), 16) }
    else if(numberString.startsWith("h")) { parseWithRadix(numberString.drop(1), 16) }
    else if(numberString.startsWith("o")) { parseWithRadix(numberString.drop(1), 8) }
    else if(numberString.startsWith("b")) { parseWithRadix(numberString.drop(1), 2) }
    else                                  { parseWithRadix(numberString, 10) }
  }
  // scalastyle:off number.of.methods
  object Commands {
    def getOneArg(failureMessage: String, argOption: Option[String] = None): Option[String] = {
      if(args.length == 2) {
        Some(args(1))
      }
      else if(args.length == 1 && argOption.isDefined) {
        Some(argOption.get)
      }
      else {
        error(failureMessage)
        None
      }
    }
    def getTwoArgs(failureMessage: String,
                   arg1Option: Option[String] = None,
                   arg2Option: Option[String] = None
                  ): (Option[String],Option[String]) = {
      if(args.length == 3) {
        (Some(args(1)), Some(args(2)))
      }
      else if(args.length == 2) {
        (Some(args(1)), arg2Option)
      }
      else if(args.length == 1) {
        (arg1Option, arg2Option)
      }
      else {
        error(failureMessage)
        (None, None)
      }
    }
    //noinspection ScalaStyle
    def getThreeArgs(failureMessage: String,
                     arg1Option: Option[String] = None,
                     arg2Option: Option[String] = None,
                     arg3Option: Option[String] = None
                  ): Option[(String,String,String)] = {
      (args.length, arg1Option, arg2Option, arg3Option) match {
        case (4, _, _, _)                             => Some(args(1), args(2), args(3))
        case (3, _, _, Some(arg3))                    => Some(args(1), args(2), arg3)
        case (2, _, Some(arg2), Some(arg3))           => Some(args(1), arg2, arg3)
        case (1, Some(arg1), Some(arg2), Some(arg3))  => Some(arg1, arg2, arg3)
        case _ =>
          error(failureMessage)
          None
      }
    }

    val commands: ArrayBuffer[Command] = ArrayBuffer.empty[Command]
    commands ++= Seq(
      new Command("load") {
        def usage: (String, String) = ("load fileName", "load/replace the current firrtl file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"load"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("load filename") match {
            case Some(fileName) => loadFile(fileName)
            case _ =>
          }
        }
      },
      new Command("script") {
        def usage: (String, String) = ("script fileName", "load a script from a text file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"script"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("script filename") match {
            case Some(fileName) => loadScript(fileName)

            case _ =>
          }
        }
      },
      new Command("run") {
        def usage: (String, String) = ("run [linesToRun|all|list|reset]", "run loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"run"}),
            new StringsCompleter(jlist(Seq("all", "reset", "list")))
          ))
        }
        def handleList(script: Script, listArg: Option[String]): Unit = {
          val (min, max) = listArg match {
            case Some(IntPattern(intString)) =>
              val windowHalfSize = intString.toInt
              (script.currentLine + 1 - windowHalfSize, script.currentLine + 2 + windowHalfSize)
            case Some(other) =>
              console.println(s"run list parameter=$other, parameter must be an positive integer")
              (0, 0)
            case _ =>
              (0, script.length)
          }
          console.println(
            script.lines.zipWithIndex.flatMap { case (line, index) =>
              if(index >= min && index < max) {
                if (index == script.currentLine + 1) {
                  Some(Console.GREEN + f"$index%3d $line" + Console.RESET)
                }
                else {
                  Some(f"$index%3d $line")
                }
              }
              else {
                None
              }
            }.mkString("\n")
          )
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          currentScript match {
            case Some(script) =>
              getTwoArgs("run [lines|skip [n]|set n|all|reset|list [n]], default is 1 => run 1 line",
                arg1Option = Some("1"), arg2Option = None) match {
                case (Some("all"), _)   =>
                  console.println("run all")
                  if(script.atEnd) { script.reset() }
                  else { script.runRemaining() }
                case (Some("reset"), _) =>
                  script.reset()
                  handleList(script, Some("2"))
                case (Some("list"), listArg) =>
                  handleList(script, listArg)
                case (Some("skip"), listArg) =>
                  val skip = listArg match {
                    case Some(IntPattern(intString)) => intString.toInt
                    case _ => 1
                  }
                  script.setSkipLines(skip)
                case (Some("set"), listArg) =>
                  listArg match {
                    case Some(IntPattern(intString)) =>
                      script.setLine(intString.toInt)
                      handleList(script, Some("2"))
                    case _ =>
                      console.println("must specify set line number")
                  }
                case (Some(IntPattern(intString)), _) =>
                  val linesToRun = intString.toInt
                  script.setLinesToRun(linesToRun)
                case (None, None) =>
                  script.runRemaining()
                case (Some(arg), _) =>
                  error(s"unrecognized run_argument $arg")
              }
            case _ =>
              error(s"No current script")
          }
        }
        // scalastyle:on cyclomatic.complexity

      },
      new Command("vcd") {
        def usage: (String, String) = ("vcd [load|run|list|test|help]", "control vcd input file")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"vcd"}),
            new ArgumentCompleter(
              new ArgumentCompleter(
                new StringsCompleter(jlist(Seq("run", "inputs", "list", "test")))
              ),
              new ArgumentCompleter(
                new StringsCompleter({"load"}),
                new FileNameCompleter
              )
            )
          ))
        }
        def run(args: Array[String]): Unit = {
          replVcdController match {
            case Some(controller) => controller.processListCommand(args)
            case _ =>
              args.toList match {
                case "load" :: fileName :: Nil =>
                  loadVcdScript(fileName)
                case _ =>
                  error(s"No current script")
              }
          }
        }
      },
      new Command("record-vcd") {
        def usage: (String, String) = ("record-vcd [<fileName>]|[done]", "firrtl_interpreter.vcd loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"record-vcd"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          getOneArg("firrtl_interpreter.vcd [fileName|done]",
            argOption = Some("out.firrtl_interpreter.vcd")) match {
            case Some("done")   =>
              interpreter.disableVCD()
            case Some(fileName) =>
              interpreter.makeVCDLogger(
                fileName, showUnderscored = optionsManager.interpreterOptions.vcdShowUnderscored)
            case _ =>
              interpreter.disableVCD()
          }
        }
      },
      new Command("type") {
        private def peekableThings = interpreter.circuitState.validNames.toSeq
        def usage: (String, String) = ("type regex", "show the current type of things matching the regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "type"
              }),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("type regex") match {
            case Some((peekRegex)) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      try {
                        val value = interpreter.getValue(settableThing)
                        console.println(s"type $settableThing $value")
                        true
                      }
                      catch { case _: Exception => false}
                    case _ =>
                      false
                  }
                }
                if(numberOfThingsPeeked == 0) {
                  console.println(s"Sorry now settable ports matched regex $peekRegex")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("poke") {
        def usage: (String, String) = ("poke inputPortName value", "set an input port to the given integer value")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "poke"
              }),
              new StringsCompleter(
                jlist(interpreter.dependencyGraph.inputPorts.toSeq ++ interpreter.dependencyGraph.registerNames)
              )
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputPortName value") match {
            case (Some(portName), Some(valueString)) =>
              try {
                val numberValue = parseNumber(valueString)
                interpreter.setValueWithBigInt(portName, numberValue)
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("mempoke") {
        def usage: (String, String) = ("mempoke memory-instance-name index value", "set memory at index to value")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "mempoke"
              }),
              new StringsCompleter(
                jlist(interpreter.circuitState.memories.keys.toSeq)
              )
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getThreeArgs("poke inputPortName value") match {
            case Some((memoryName, indexString, valueString)) =>
              try {
                val index = indexString.toInt
                val value = parseNumber(valueString)
                interpreter.setMemory(memoryName, index, value)              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("rpoke") {
        private def settableThings = {
          interpreter.dependencyGraph.inputPorts.toSeq ++ interpreter.dependencyGraph.registerNames
        }
        def usage: (String, String) = ("rpoke regex value", "poke value into ports that match regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "rpoke"
              }),
              new StringsCompleter(jlist(settableThings))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("rpoke regex value") match {
            case (Some(pokeRegex), Some(valueString)) =>
              try {
                val pokeValue = parseNumber(valueString)
                val portRegex = pokeRegex.r
                val setThings = settableThings.flatMap { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      interpreter.setValueWithBigInt(settableThing, pokeValue)
                      Some(settableThing)
                    case _ => None
                  }
                }
                if(setThings.nonEmpty) {
                  console.println(s"poking value $pokeValue into ${setThings.toList.sorted.mkString(", ")}")
                }
                else {
                  console.println(s"Sorry now settable ports matched regex $pokeRegex")
                }


              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("eval") {
        def usage: (String, String) = ("eval componentName", "show the computation of the component")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "eval"
              }),
              new StringsCompleter(jlist(interpreter.circuitState.validNames.toSeq))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("eval componentName") match {
            case Some(componentName) =>
              try {
                val saveVerbose = interpreter.verbose
                val saveExec = interpreter.evaluator.useTopologicalSortedKeys
                interpreter.evaluator.useTopologicalSortedKeys = true
                interpreter.setVerbose()
                interpreter.evaluateCircuit(Seq(componentName))
                interpreter.setVerbose(saveVerbose)
                interpreter.evaluator.useTopologicalSortedKeys = saveExec
              }
              catch {
                case e: Exception =>
                  sys.error(s"exception ${e.getMessage}")
                case a: AssertionError =>
                  sys.error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("peek") {
        def usage: (String, String) = ("peek componentName", "show the current value of the named circuit component")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "peek"
              }),
              new StringsCompleter(jlist(interpreter.circuitState.validNames.toSeq))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("peek componentName") match {
            case Some(componentName) =>
              try {
                val value = interpreter.getSpecifiedValue(componentName)
                console.println(s"peek $componentName ${value.showValue}")
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage}")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("mempeek") {
        def usage: (String, String) = ("mempeek memory-instance-name index", "peek memory at index")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "mempeek"
              }),
              new StringsCompleter(
                jlist(interpreter.circuitState.memories.keys.toSeq)
              )
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputPortName value") match {
            case (Some(memoryName), Some(indexString)) =>
              try {
                val index = indexString.toInt
                val value = interpreter.getMemoryConcrete(memoryName, index)
                console.println(s"peek $memoryName($index)  ${value.showValue}")
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("rpeek") {
        private def peekableThings = interpreter.circuitState.validNames.toSeq
        def usage: (String, String) = ("rpeek regex", "show the current value of things matching the regex")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "rpeek"
              }),
              new StringsCompleter(jlist(peekableThings))
            ))
          }
        }
        //scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("rpeek regex") match {
            case Some((peekRegex)) =>
              try {
                val portRegex = peekRegex.r
                val numberOfThingsPeeked = peekableThings.sorted.count { settableThing =>
                  portRegex.findFirstIn(settableThing) match {
                    case Some(_) =>
                      try {
                        val value = interpreter.getSpecifiedValue(settableThing)
                        console.println(s"rpeek $settableThing ${value.showValue}")
                        true
                      }
                      catch { case _: Exception => false}
                    case _ =>
                      false
                  }
                }
                if(numberOfThingsPeeked == 0) {
                  console.println(s"Sorry now settable ports matched regex $peekRegex")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
                case a: AssertionError =>
                  error(s"exception ${a.getMessage}")
              }
            case _ =>
          }
        }
      },
      new Command("randomize") {
        def usage: (String, String) = ("randomize",
          "randomize all inputs except reset)")
        def run(args: Array[String]): Unit = {
          for{
            (component, value) <- interpreter.circuitState.inputPorts ++
              interpreter.circuitState.outputPorts ++
              interpreter.circuitState.ephemera
          } {
            try {
              val newValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = false)
              interpreter.setValue(component, newValue)
              console.println(s"setting $component to $newValue")
            }
            catch {
              case e: Exception =>
                console.println(s"Error randomize: setting $component to $value error ${e.getMessage}")
            }
          }
          for((component, value) <- interpreter.circuitState.registers) {
            try {
              val newValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = false)
              interpreter.circuitState.registers(component) = newValue
              val newNextValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = false)
              interpreter.circuitState.nextRegisters(component) = newNextValue
              console.println(s"setting $component to $newValue")
            }
            catch {
              case e: Exception =>
                console.println(s"Error randomize: setting $component to $value error ${e.getMessage}")
            }
          }
          for(memory <- interpreter.circuitState.memories.values) {
            for(memoryIndex <- 0 until memory.dataStore.length) {
              memory.dataStore.update(
                memoryIndex,
                TypeInstanceFactory.makeRandomSimilar(memory.dataStore.underlyingData.head, poisoned = false))
            }
          }
          console.println(interpreter.circuitState.prettyString())
        }
      },
      new Command("poison") {
        def usage: (String, String) = ("poison",
          "poison everything)")
        def run(args: Array[String]): Unit = {
          for{
            (component, value) <- interpreter.circuitState.inputPorts ++
              interpreter.circuitState.outputPorts ++
              interpreter.circuitState.ephemera
          } {
            interpreter.setValue(component, TypeInstanceFactory.makeRandomSimilar(value, poisoned = true))
          }
          for((component, value) <- interpreter.circuitState.registers) {
            try {
              val newValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = true)
              interpreter.circuitState.registers(component) = newValue
              val newNextValue = TypeInstanceFactory.makeRandomSimilar(value, poisoned = true)
              interpreter.circuitState.nextRegisters(component) = newNextValue
              console.println(s"setting $component to $newValue")
            }
            catch {
              case e: Exception =>
                console.println(s"Error poison: setting $component to $value error ${e.getMessage}")
            }
          }
          for(memory <- interpreter.circuitState.memories.values) {
            for(memoryIndex <- 0 until memory.dataStore.length) {
              memory.dataStore.update(memoryIndex, TypeInstanceFactory(memory.dataType))
            }
          }
          console.println(interpreter.circuitState.prettyString())
        }
      },
      new Command("reset") {
        def usage: (String, String) = ("reset [numberOfSteps]",
          "assert reset (if present) for numberOfSteps (default 1)")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "reset"
              })
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("reset [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                interpreter.setValueWithBigInt("reset", 1)
                val numberOfSteps = numberOfStepsString.toInt
                for(_ <- 0 until numberOfSteps) {
                  interpreter.cycle()
                  interpreter.evaluateCircuit()
                }
                interpreter.setValueWithBigInt("reset", 0)
                // console.println(interpreter.circuitState.prettyString())
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("step") {
        def usage: (String, String) = ("step [numberOfSteps]",
          "cycle the clock numberOfSteps (default 1) times, and show state")
        def run(args: Array[String]): Unit = {
          getOneArg("step [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                val numberOfSteps = numberOfStepsString.toInt
                interpreter.timer("steps") {
                  for (_ <- 0 until numberOfSteps) {
                    interpreter.timer("step") {
                      interpreter.cycle()
                      interpreter.evaluateCircuit()
                    }
                  }
                }
                if(! scriptRunning) {
                  // console.println(interpreter.circuitState.prettyString())
                  console.println(s"step $numberOfSteps in ${interpreter.timer.prettyLastTime("steps")}")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("waitfor") {
        def usage: (String, String) = ("waitfor componentName value [maxNumberOfSteps]",
          "wait for particular value (default 1) on component, up to maxNumberOfSteps (default 100)")
        def run(args: Array[String]): Unit = {
          getThreeArgs(
            "waitfor componentName [value] [maxNumberOfSteps]",
            arg2Option = Some("1"),
            arg3Option = Some("100")
          ) match {
            case Some((componentName, valueString, maxNumberOfStepsString)) =>
              try {
                val maxNumberOfSteps = maxNumberOfStepsString.toInt
                val value = valueString.toInt

                var tries = 0
                while(tries < maxNumberOfSteps && interpreter.getValue(componentName).value != BigInt(value)) {
                  interpreter.cycle()
                  tries += 1
                }
                if(interpreter.getValue(componentName).value != BigInt(value)) {
                  console.println(
                    s"waitfor exhausted $componentName did not take on value $value in $maxNumberOfSteps cycles")
                }
                else {
                  console.println(s"$componentName == value $value in $tries cycles")
                }
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
              }
            case _ =>
          }
        }
      },
      new Command("show") {
        def usage: (String, String) = ("show [state|input|lofirrtl]", "show useful things")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "show"}),
              new StringsCompleter(jlist(Seq("state", "input", "lofirrtl")))
            ))
          }
        }

        def run(args: Array[String]): Unit = {
          getOneArg("", Some("state")) match {
            case Some("lofirrtl") =>
              console.println(interpreter.loweredAst.serialize)
            case Some("input") =>
              console.println(interpreter.ast.serialize)
            case _ =>
              console.println(interpreter.circuitState.prettyString())
          }
        }
      },
      new Command("info") {
        def usage: (String, String) = ("info", "show information about the circuit")
        def run(args: Array[String]): Unit = {
          console.println(interpreter.dependencyGraph.getInfo)
        }
      },
      new Command("timing") {
        def usage: (String, String) = ("timing [clear|bin]", "show the current timing state")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "timing"}),
              new StringsCompleter(jlist(Seq("clear", "bin")))
            ))
          }
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          getOneArg("", Some("")) match {
            case Some("clear") => interpreter.timer.clear()
            case Some("bin") =>
              val names = interpreter.dependencyGraph.validNames -- interpreter.dependencyGraph.inputPorts

              val countPerName = new scala.collection.mutable.HashMap[Long, Long]
              names.foreach { name =>
                interpreter.timer.timingLog.get(name).foreach { t =>
                  if(! countPerName.contains(t.events)) {
                    countPerName(t.events) = 1
                  }
                  else {
                    countPerName(t.events) = countPerName(t.events) + 1
                  }
                }
              }
              countPerName.keys.toSeq.sorted.foreach { count: Long =>
                console.println(f"$count ${countPerName(count)}")
              }
            case _ =>
              val names = interpreter.dependencyGraph.validNames -- interpreter.dependencyGraph.inputPorts

              val sortedNames = names.toSeq.sortWith { case (a, b) =>
                (interpreter.timer.timingLog.get(a), interpreter.timer.timingLog.get(b)) match {
                  case (Some(t1), Some(t2)) =>
                    if(t1.events == t2.events) {
                      a < b
                    }
                    else {
                      t1.events < t2.events
                    }
                  case (Some(_), None)      => false
                  case (None, Some(_))      => true
                  case _                    => a < b
                }
              }
              for (name <- sortedNames) {
                console.println(f"$name%-20s ${interpreter.timer.prettyEntryForTag(name)}")
              }
              console.println(f"${"Total"}%-20s ${interpreter.timer.prettyEntry(interpreter.timer.totalEvent)}")
          }
        }
      },
      new Command("verbose") {
        def usage: (String, String) = ("verbose [true|false|toggle]",
          "set evaluator verbose mode (default toggle) during dependency evaluation")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "verbose"}),
              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("verbose must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") => interpreter.setVerbose(! interpreter.verbose)
            case Some("true")   => interpreter.setVerbose()
            case Some("false")  => interpreter.setVerbose(false)
            case _ =>
          }
          console.println(s"evaluator verbosity is now ${interpreter.verbose}")
        }
      },
      new Command("eval-all") {
        def usage: (String, String) = ("eval-all [true|false|toggle]",
          "set evaluator to execute un-needed branches (default toggle) during dependency evaluation")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "eval-all"}),
              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("eval-all must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") => interpreter.evaluator.evaluateAll = ! interpreter.evaluator.evaluateAll
            case Some("true")   => interpreter.evaluator.evaluateAll = true
            case Some("false")  => interpreter.evaluator.evaluateAll = false
            case _ =>
          }
          console.println(s"evaluator verbosity is now ${interpreter.evaluator.evaluateAll}")
        }
      },
      new Command("allow-cycles") {
        def usage: (String, String) = ("allow-cycles [true|false|toggle]",
          "set evaluator allow combinational loops (could cause correctness problems")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "allow-cycles"}),
              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("allow-cycles must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") =>
              interpreter.evaluator.allowCombinationalLoops = ! interpreter.evaluator.allowCombinationalLoops
            case Some("true")   => interpreter.evaluator.allowCombinationalLoops = true
            case Some("false")  => interpreter.evaluator.allowCombinationalLoops = false
            case _ =>
          }
          console.println(s"evaluator allow combinational loops is now ${interpreter.evaluator.evaluateAll}")
        }
      },
      new Command("ordered-exec") {
        def usage: (String, String) = ("ordered-exec [true|false|toggle]",
          "set evaluator execute circuit in dependency order, now recursive component evaluation")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpreterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({ "ordered-exec"}),
              new StringsCompleter(jlist(Seq("true", "false", "toggle")))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getOneArg("ordered-exec must be followed by true false or toggle", Some("toggle")) match {
            case Some("toggle") =>
              interpreter.evaluator.useTopologicalSortedKeys = ! interpreter.evaluator.useTopologicalSortedKeys
            case Some("true")   => interpreter.evaluator.useTopologicalSortedKeys = true
            case Some("false")  => interpreter.evaluator.useTopologicalSortedKeys = false
            case _ =>
          }
          if(interpreter.evaluator.useTopologicalSortedKeys) {
            interpreter.setValueWithBigInt("reset", 1)
            interpreter.evaluator.evaluateAll = true
            interpreter.cycle()
            interpreter.evaluateCircuit()
            interpreter.setValueWithBigInt("reset", 0)
            interpreter.evaluator.evaluateAll = true
          }
          console.println(s"evaluator ordered-exec is now ${interpreter.evaluator.useTopologicalSortedKeys}")
        }
      },
      new Command("help") {
        def usage: (String, String) = ("help", "show available commands")
        def run(args: Array[String]): Unit = {
          val maxColumn1Width = Commands.commands.map(_.usage._1.length).max + 2
          Commands.commands.foreach { command =>
            val (column1, column2) = command.usage
            terminal.getWidth

            console.println(s"$column1${" "*(maxColumn1Width - column1.length)} $column2")
          }
        }
      },
      new Command("quit") {
        def usage: (String, String) = ("quit", "exit the interpreter")
        def run(args: Array[String]): Unit = {
          if(! history.isEmpty) {
            history.removeLast()
          }
          done = true
        }
      }
    )
    val commandMap: Map[String, Command] = commands.map(command => command.name -> command).toMap
  }
  //scalastyle:on

  def buildCompletions(): Unit = {
    console.setCompletionHandler(new CandidateListCompletionHandler {})
    Commands.commands.flatMap { command =>
      command.completer
    }.foreach { completer =>
      console.addCompleter(completer)
    }
  }

  /**
    * gets the next line from either the current executing script or from the console.
    * Strips comments from the line, may result in empty string, command parser is ok with that
 *
    * @return
    */
  def getNextLine: String = {
    val rawLine = currentScript match {
      case Some(script) =>
        script.getNextLineOption match {
          case Some(line) =>
            console.println(s"[${script.currentLine}:${script.fileName}] $line")
            line
          case _ =>
            console.readLine()
        }
      case _ =>
        console.readLine()
    }
    if(rawLine == null) {
      "quit"
    }
    else {
      rawLine.split("#").head
    }
  }

  def scriptRunning: Boolean = {
    currentScript match {
      case Some(script) => script.hasNext
      case _            => false
    }
  }

  //scalastyle:off method.length
  def run() {
    if(replConfig.firrtlSource.nonEmpty) {
      loadSource(replConfig.firrtlSource)
    }
    else if(replConfig.firrtlSourceName.nonEmpty) {
      loadFile(replConfig.firrtlSourceName)
    }
    else if(optionsManager.commonOptions.programArgs.nonEmpty) {
      loadFile(optionsManager.commonOptions.programArgs.head)
    }
    if(replConfig.scriptName.nonEmpty) {
      loadScript(replConfig.scriptName)
    }
    if(replConfig.useVcdScript) {
      loadVcdScript(optionsManager.getVcdFileName)
    }
    buildCompletions()

    console.setPrompt("firrtl>> ")

    if(replConfig.runScriptAtStart) {
      currentScript match {
        case Some(script) =>
          script.reset()
          script.runRemaining()
        case None =>
          console.println(s"Error: fr-run-script-at-startup set, with no script file")
      }
    }

    while (! done) {
      try {
        val line = getNextLine

        line.split(""";""").foreach { subLine =>

          args = subLine.trim.split(" +")

          if (args.length > 0) {
            if (Commands.commandMap.contains(args.head)) {
              Commands.commandMap(args.head).run(args.tail)
            }
            else {
              if (subLine.nonEmpty) error(s"unknown command $subLine, try help")
            }
          }
          else {
            error(s"unknown command: $subLine")
          }
        }
      }
      catch {
        case ie: InterpreterException =>
          console.println(s"Interpreter Exception occurred: ${ie.getMessage}")
          ie.printStackTrace()
        case e: NullPointerException =>
          error(s"Null pointer exception, please file an issue\n ${e.getMessage}")
          e.printStackTrace()
        case e: Exception =>
          console.println(s"Exception occurred: ${e.getMessage}")
          e.printStackTrace()
      }
    }

    console.println(s"saving history ${history.size()}")
    console.flush()
    history.flush()
    console.shutdown()
    terminal.restore()
  }

  def error(message: String): Unit = {
    console.println(s"Error: $message")
  }

  def showFlags: String = {
    s"allow-cycles: ${interpreter.evaluator.allowCombinationalLoops} " +
    s"ordered-exec: ${interpreter.evaluator.useTopologicalSortedKeys}"
  }

  def jlist(list: Seq[String]): java.util.List[String]= {
    val array = ArrayBuffer.empty[String]
    array ++= list
    array.asJava
  }
}

object FirrtlRepl {
  def execute(optionsManager: InterpreterOptionsManager with HasReplConfig): Unit = {
    val repl = new FirrtlRepl(optionsManager)
    repl.run()
  }

  def main(args: Array[String]): Unit = {
    val optionsManager = new InterpreterOptionsManager with HasReplConfig

    if(optionsManager.parse(args)) {
      val repl = new FirrtlRepl(optionsManager)
      repl.run()
    }
  }
}
