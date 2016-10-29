// See LICENSE for license details.
package firrtl_interpreter

import java.io.File

import firrtl.ExecutionOptionsManager

import scala.collection.mutable.ArrayBuffer
import scala.tools.jline.console.ConsoleReader
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.TerminalFactory
import scala.tools.jline.console.completer._
import collection.JavaConverters._

abstract class Command(val name: String) {
  def run(args: Array[String])
  def usage: (String, String)
  def completer: Option[ArgumentCompleter] = {
    Some(new ArgumentCompleter(
      new StringsCompleter({name})
    ))
  }
}

class FirrtlRepl(optionsManager: ExecutionOptionsManager with HasReplConfig with HasInterpreterOptions) {
  val replConfig = optionsManager.replConfig
  val interpreterOptions = optionsManager.interpreterOptions

  firrtl_interpreter.random.setSeed(interpreterOptions.randomSeed)

  val terminal = TerminalFactory.create()
  val console = new ConsoleReader
  val historyPath = "~/.firrtl_repl_history".replaceFirst("^~",System.getProperty("user.home"))
  val historyFile = new File(historyPath)
  if(! historyFile.exists()) {
    println(s"creating ${historyFile.getName}")
    historyFile.createNewFile()
  }
  val history = new FileHistory(historyFile)

  history.load(historyFile)
  console.setHistory(history)

  var currentInterpeterOpt: Option[FirrtlTerp] = None

  def interpreter: FirrtlTerp = currentInterpeterOpt.get
  var args = Array.empty[String]
  var done = false

  var inScript = false
  val scriptFactory = ScriptFactory(this)
  var currentScript: Option[Script] = None
  val intPattern = """(-?\d+)""".r

  def loadSource(input: String): Unit = {
    currentInterpeterOpt = Some(FirrtlTerp(input, blackBoxFactories = interpreterOptions.blackBoxFactories))
    currentInterpeterOpt.foreach { _=>
      interpreter.evaluator.allowCombinationalLoops = interpreterOptions.allowCycles
      interpreter.evaluator.useTopologicalSortedKeys = interpreterOptions.setOrderedExec
      interpreter.evaluator.evaluationStack.MaxExecutionDepth = interpreterOptions.maxExecutionDepth
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
    loadSource(input)
  }

  def loadScript(fileName: String): Unit = {
    currentScript = scriptFactory(fileName)
    currentScript match {
      case Some(script) =>
        console.println(s"loaded script file ${script.length} with ${script.fileName} lines")
      case _ =>
    }
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
                  ): Option[(String,String)] = {
      if(args.length == 3) {
        Some(args(1), args(2))
      }
      else if(args.length == 2 && arg2Option.isDefined) {
        Some(args(1), arg2Option.get)
      }
      else if(args.length == 1 && arg1Option.isDefined && arg2Option.isDefined) {
        Some(arg1Option.get, arg2Option.get)
      }
      else {
        error(failureMessage)
        None
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

    val commands = ArrayBuffer.empty[Command]
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
        def usage: (String, String) = ("run [linesToRun|all|reset]", "run loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"run"}),
            new StringsCompleter(jlist(Seq("all", "reset", "list")))
          ))
        }
        // scalastyle:off cyclomatic.complexity
        def run(args: Array[String]): Unit = {
          currentScript match {
            case Some(script) =>
              getOneArg("run [linesToRun|all|reset|list]", argOption = Some("all")) match {
                case Some("all")   =>
                  console.println("run all")
                  if(script.atEnd) { script.reset() }
                  else { script.runRemaining() }
                case Some("reset") => script.reset()
                  console.println("run reset")
                case Some("list") =>
                  console.println(
                    script.lines.zipWithIndex.map { case (line, index) =>
                      f"$index%3d $line"
                    }.mkString("\n")
                  )
                case Some(intPattern(intString)) =>
                  console.println(s"run $intString")
                  val linesToRun = intString.toInt
                  script.setLinesToRun(linesToRun)
                case None =>
                  script.runRemaining()
                case Some(arg) =>
                  error(s"unrecognized run_argument $arg")
              }
            case _ =>
              error(s"No current script")
          }
        }
        // scalastyle:on cyclomatic.complexity

      },
      new Command("vcd") {
        def usage: (String, String) = ("firrtl_interpreter.vcd fileName|[done]", "firrtl_interpreter.vcd loaded script")
        override def completer: Option[ArgumentCompleter] = {
          Some(new ArgumentCompleter(
            new StringsCompleter({"vcd"}),
            new FileNameCompleter
          ))
        }
        def run(args: Array[String]): Unit = {
          currentScript match {
            case Some(script) =>
              getOneArg("firrtl_interpreter.vcd [fileName|done]",
                argOption = Some("out.firrtl_interpreter.vcd")) match {
                case Some("done")   =>
                  interpreter.disableVCD()
                case Some(fileName) =>
                  interpreter.makeVCDLogger(fileName)
                case _ =>
                  interpreter.disableVCD()
              }
            case _ =>
              error(s"No current script")
          }
        }
      },
      new Command("poke") {
        def usage: (String, String) = ("poke inputPortName value", "set an input port to the given integer value")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpeterOpt.isEmpty) {
            None
          }
          else {
            Some(new ArgumentCompleter(
              new StringsCompleter({
                "poke"
              }),
              new StringsCompleter(jlist(interpreter.dependencyGraph.inputPorts.toSeq))
            ))
          }
        }
        def run(args: Array[String]): Unit = {
          getTwoArgs("poke inputPortName value") match {
            case Some((portName, valueString)) =>
              try {
                if(valueString.startsWith("0x")) {
                  val hexValue = BigInt(valueString.drop(2), 16)
                  interpreter.setValueWithBigInt(portName, hexValue)
                }
                else {
                  val value = valueString.toInt
                  interpreter.setValueWithBigInt(portName, value)
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
      new Command("peek") {
        def usage: (String, String) = ("peek componentName", "show the current value of the named circuit component")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpeterOpt.isEmpty) {
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
                val value = interpreter.getValue(componentName)
                console.println(s"peek $componentName $value")
              }
              catch {
                case e: Exception =>
                  error(s"exception ${e.getMessage} $e")
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
            (inputPortName, value) <- interpreter.circuitState.inputPorts
            if inputPortName != "reset"
          } {
            interpreter.setValue(inputPortName, TypeInstanceFactory(value, randomBigInt(value.width)))
          }
          console.println(interpreter.circuitState.prettyString())
        }
      },
      new Command("reset") {
        def usage: (String, String) = ("reset [numberOfSteps]",
          "assert reset (if present) for numberOfSteps (default 1)")
        def run(args: Array[String]): Unit = {
          getOneArg("reset [numberOfSteps]", Some("1")) match {
            case Some(numberOfStepsString) =>
              try {
                interpreter.setValueWithBigInt("reset", 1)
                val numberOfSteps = numberOfStepsString.toInt
                for(stepNumber <- 0 until numberOfSteps) {
                  interpreter.cycle(showState = false)
                  interpreter.evaluateCircuit()
                }
                interpreter.setValueWithBigInt("reset", 0)
                console.println(interpreter.circuitState.prettyString())
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
                  for (stepNumber <- 0 until numberOfSteps) {
                    interpreter.timer("step") {
                      interpreter.cycle(showState = false)
                      interpreter.evaluateCircuit()
                    }
                  }
                }
                if(! scriptRunning) {
                  console.println(interpreter.circuitState.prettyString())
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
        def usage: (String, String) = ("show", "show the state of the circuit")
        def run(args: Array[String]): Unit = {
          console.println(interpreter.circuitState.prettyString())
        }
      },
      new Command("timing") {
        def usage: (String, String) = ("timing [clear|bin]", "show the current timing state")
        override def completer: Option[ArgumentCompleter] = {
          if(currentInterpeterOpt.isEmpty) {
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
                  case (Some(t1), None)     => false
                  case (None, Some(t2))     => true
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
          if(currentInterpeterOpt.isEmpty) {
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
            case Some("true")   => interpreter.setVerbose(true)
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
          if(currentInterpeterOpt.isEmpty) {
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
          if(currentInterpeterOpt.isEmpty) {
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
          if(currentInterpeterOpt.isEmpty) {
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
            interpreter.cycle(showState = false)
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
          history.removeLast()
          done = true
        }
      }
    )
    val commandMap = commands.map(command => command.name -> command).toMap
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

  def getNextLine: String = {
    currentScript match {
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
  }

  def scriptRunning: Boolean = {
    currentScript match {
      case Some(script) => script.hasNext
      case _            => false
    }
  }


  def run() {
    if(replConfig.firrtlSource.nonEmpty) {
      loadSource(replConfig.firrtlSource)
    }
    else if(replConfig.firrtlSourceName.nonEmpty) {
      loadFile(replConfig.firrtlSourceName)
    }
    if(replConfig.scriptName.nonEmpty) {
      loadScript(replConfig.scriptName)
    }

    buildCompletions()

    console.setPrompt("firrtl>> ")

    while (! done) {
      try {

//        val line = console.readLine()
        val line = getNextLine

        args = line.split(" ")

        if (args.length > 0) {
          if (Commands.commandMap.contains(args.head)) {
            Commands.commandMap(args.head).run(args.tail)
          }
          else {
            error(s"unknown command $line, try help")
          }
        }
        else {
          error("unknown command")
        }
      }
      catch {
        case ie: InterpreterException =>
          console.println(s"Interpreter Exception occurred: ${ie.getMessage}")
        case e: NullPointerException =>
          error(s"repl error ${e.getMessage}")
          done = true
        case e: Exception =>
          console.println(s"Exception occurred: ${e.getMessage}")
      }
    }
    console.println(s"saving history ${history.size()}")
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
  def execute(optionsManager: ExecutionOptionsManager with HasReplConfig with HasInterpreterOptions): Unit = {
    val repl = new FirrtlRepl(optionsManager)
    repl.run()
  }

  def main(args: Array[String]): Unit = {
    val optionsManager = new ExecutionOptionsManager("firrtl-repl") with HasReplConfig with HasInterpreterOptions

    optionsManager.parse(args) match {
      case true =>
        val repl = new FirrtlRepl(optionsManager)
        repl.run()
      case _ =>
    }
  }
}
