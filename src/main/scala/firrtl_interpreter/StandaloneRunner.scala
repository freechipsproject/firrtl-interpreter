// See LICENSE for license details.

package firrtl_interpreter

import firrtl.ExecutionOptionsManager

class StandaloneRunner(
    manager: InterpreterOptionsManager with HasStandaloneConfig,
    firrtlSource: String,
    testVectors: TestVectors
) {
  private val tester = new InterpretiveTester(firrtlSource, manager)
  private val verbose = manager.standaloneConfig.showOperations

  def run() {
    testVectors.operations.zipWithIndex.foreach { case (line, lineNumber) =>
      def parseNum(numString: String): BigInt = {
        val num = try {
          BigInt(numString)
        }
        catch {
          case e: Throwable =>
            println(s"Error in vectors at $lineNumber, could not parse numberr $numString")
            System.exit(1)
            throw e
        }
        num
      }

      def show(message: => String): Unit = {
        if(verbose) println(message)
      }

      line.split(",").map(_.trim.toLowerCase()).toList match {
        case "peek" :: portName :: _ =>
          tester.peek(portName)
          show(s"$line, ${tester.peek(portName)}")

        case "poke" :: portName :: value :: _ =>
          tester.poke(portName, parseNum(value))
          show(line)

        case "step" :: value :: _ =>
          tester.step(parseNum(value).toInt)
          show(line)

        case "step" :: Nil =>
          tester.step()
          show(line)

        case "expect" :: portName :: value :: _ =>
          tester.expect(portName, parseNum(value))
          show(line)

        case _ =>
      }
    }
    tester.report()
  }
}

case class StandaloneConfig(firrtlFileName: String = "", vectorFileName: String = "", showOperations: Boolean = false)

trait HasStandaloneConfig {
  self: ExecutionOptionsManager =>

  var standaloneConfig = StandaloneConfig()

  parser.note("standalone")

  parser.opt[String]("firrtl-source")
    .abbr("f")
    .valueName("<firrtl-source-file>")
    .foreach { x =>
       standaloneConfig = standaloneConfig.copy(firrtlFileName = x)
    }
    .text("firrtl file to execut, no default")

  parser.opt[String]("test-vector-file")
    .abbr("tv")
    .valueName("<test-vector-file>")
    .foreach { x =>
      standaloneConfig = standaloneConfig.copy(vectorFileName = x)
    }
    .text("test vector file, no default")

  parser.opt[Unit]("show-operations")
    .abbr("so")
    .foreach { _ =>
      standaloneConfig = standaloneConfig.copy(showOperations = true)
    }
    .text("Currently unsupported, use --fr-vcd-script-override <vcd-file-name> instead")

}

object StandaloneRunner {
  private def usage(): Unit = {
    println(s"Usage: StandaloneRunner -f <firrtl-source> -tv <vector-file> [-")
    println(s"       Use --help to see more options")
    System.exit(1)
  }

  def main(args: Array[String]): Unit = {
    val optionsManager = new InterpreterOptionsManager with HasStandaloneConfig
    optionsManager.doNotExitOnHelp()

    if (optionsManager.parse(args)) {
      val sourceFile = optionsManager.standaloneConfig.firrtlFileName
      val vectorFile = optionsManager.standaloneConfig.vectorFileName
      if(sourceFile.isEmpty || vectorFile.isEmpty) usage()

      val testVectors = try {
        TestVectors(vectorFile)
      }
      catch {
        case e: Throwable =>
          println(s"Error:could not read source ${new java.io.File(vectorFile).getAbsolutePath}")
          usage()
          throw e
      }

      val firrtlSource = try {
        io.Source.fromFile(sourceFile).getLines().mkString("\n")
      }
      catch {
        case e: Throwable =>
          println(s"Error:could not read source ${new java.io.File(sourceFile).getAbsolutePath}")
          usage()
          throw e
      }

      val runner = new StandaloneRunner(optionsManager, firrtlSource, testVectors)
      runner.run()    }
  }
}

class TestVectors(val operations: Seq[String])

object TestVectors {
  def apply(file: String): TestVectors = {
    new TestVectors(io.Source.fromFile(file).getLines().toSeq)
  }
}

