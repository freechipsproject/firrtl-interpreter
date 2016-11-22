// See LICENSE for license details.

package firrtl_interpreter.vcd

import java.io.PrintWriter
import java.text.SimpleDateFormat

import logger.LazyLogging

import collection._
import java.util.{Date, TimeZone}

import scala.collection.mutable.ArrayBuffer

object VCD extends LazyLogging {
  val Version = "0.2"

  val DateDeclaration = "$date"
  val VersionDeclaration = "$version"
  val CommentDeclaration = "$comment"
  val TimeScaleDeclaration = "$timescale"
  val ScopeDeclaration = "$scope"
  val VarDeclaration = "$var"
  val UpScopeDeclaration = "$upscope"
  val EndDefinitionsDeclaration = "$enddefinitions"
  val DumpVarsDeclaration = "$dumpvars"
  val End = "$end"

  val idChars = (33 to 126).map { asciiValue => asciiValue.toChar.toString }
  val numberOfIdChars = idChars.length

  // A number of regular expressions to parse vcd lines
  val SectionHeader = """^\$([^\$]+) *$""".r
  val EndSection = """^\$end *$""".r

  val ScopedModule = """\s*(?i)(\S+)\s+(\S+)\s*""".r
  val JustScoped = """\s*(\S+)\s*""".r

  val VarSpec = """\s*(\w+)\s+(\d+)\s+(\S+)\s+([\S ]+)\s*""".r
  val ValueChangeScalar = """\s*(\d+)(\S+)\s*""".r
  val ValueChangeVector = """\s*([rbh])([0-9\.]+)s*""".r
  val TimeStamp = """\s*#(\d+)\s*""".r

  def apply(moduleName: String, timeScale: String = "1ps", comment: String = ""): VCD = {
    val tz = TimeZone.getTimeZone("UTC")
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mmZ")
    df.setTimeZone(tz)
    val nowAsISO = df.format(new Date())

    new VCD(
      nowAsISO,
      VCD.Version,
      comment,
      timeScale,
      moduleName
    )
  }

  class WordIterator(fileName: String) extends Iterator[String] {
    val lines = io.Source.fromFile(fileName).getLines()
    var currentLineNumber = 0
    var currentLine: Iterator[String] = Iterator.empty
    var _hasNext = false
    def hasNext: Boolean = _hasNext
    var nextWord = ""

    def next: String = {
      val lastWord = nextWord
      loadNextWord()
      lastWord
    }

    loadNextWord()

    def loadNextWord(): Unit = {
      if(currentLine.hasNext) {
        nextWord = currentLine.next()
        if(nextWord.isEmpty) {
          loadNextWord()
        }
        else {
          _hasNext = true
        }
      }
      else {
        if(lines.hasNext) {
          currentLineNumber += 1
          currentLine = lines.next().trim.split("""\s+""").toIterator
          loadNextWord()
        }
        else {
          _hasNext = false
          nextWord = ""
        }
      }
    }
  }

  //scalastyle:off cyclomatic.complexity method.length
  /**
    * Read and parse the specified vcd file, producing a VCD data structure
 *
    * @param vcdFile name of file to parse
    *                @param varPrefix only retain vars that contain prefix, remove prefix while recording
    * @return a populated VCD class
    */
  def read(
      vcdFile: String,
      startScope: String = "",
      renameStartScope: String = "",
      varPrefix: String = "",
      newVarPrefix: String = ""): VCD = {
    val words = new WordIterator(vcdFile)

    val dateHeader = new StringBuilder
    val versionHeader = new StringBuilder
    val commentHeader = new StringBuilder
    val timeScaleHeader = new StringBuilder
    val scopeBuffer = new StringBuilder
    val endDefinition = new StringBuilder
    val currentVar = new StringBuilder

    var scopeRoot: Option[Scope] = None
    var currentScope: Option[Scope] = None
    var desiredScopeFound = false

    val supportedVectorRadix = Map(
      "b" -> 2,
      "o" -> 8,
      "h" -> 16
    )

    val wires = new mutable.HashMap[String, Wire]
    val aliasedWires = new mutable.HashMap[String, mutable.HashSet[Wire]] {
      override def default(key: String): mutable.HashSet[Wire] = {
        this(key) = new mutable.HashSet[Wire]
        this(key)
      }
    }
    val skippedWires = new mutable.HashMap[String, Wire]

    var currentTime = -1L
    val valuesAtTime = new mutable.HashMap[Long, mutable.HashSet[Change]] {
      override def default(key: Long): mutable.HashSet[Change] = {
        this(key) = new mutable.HashSet[Change]
        this(key)
      }
    }

    def addScope(name: String): Unit = {
      currentScope match {
        case Some(scope) =>
          currentScope = Some(Scope(name, currentScope))
          scope.subScopes += currentScope.get
        case  _ =>
          if(startScope.isEmpty || name == startScope) {
            val newName = if(renameStartScope.isEmpty) name else renameStartScope
            scopeRoot = Some(Scope(newName))
            currentScope = scopeRoot
            desiredScopeFound = true
          }
      }
    }

    def processScope(): Unit = {
      if(words.hasNext) {
        words.next match {
          case EndSection() =>
            scopeBuffer.toString() match {
              case ScopedModule(kind, moduleName) =>
                if(kind != "module") {
                  logger.debug(s"unsupported scope type ${scopeBuffer.toString()} at line ${words.currentLineNumber}")
                }
                addScope(moduleName)
              case _ =>
                logger.warn(s"unknown scope format ${scopeBuffer.toString()} at line ${words.currentLineNumber}")
            }
            scopeBuffer.clear()
          case text =>
            scopeBuffer.append(s" $text")
            processScope()
        }
      }
    }

    def processUpScope(): Unit = {
      if(words.hasNext) {
        words.next match {
          case EndSection() =>
            currentScope = currentScope match {
              case Some(scope) => scope.parent
              case None =>
                desiredScopeFound = false
                None
            }
          case text =>
            processScope()
        }
      }
    }

    def scopePathString(scopeOption: Option[Scope], path: String = ""): String = {
      scopeOption match {
        case Some(scope) => scopePathString(scope.parent) + scope.name + "."
        case None => ""
      }
    }

    def scopePath(scopeOption: Option[Scope]): List[String] = {
      def walkPath(scopeOption: Option[Scope]): List[String] = {
        scopeOption match {
          case Some(scope) =>
            scope.name :: walkPath(scope.parent)
          case None =>
            Nil
        }
      }
      walkPath(scopeOption).reverse match {
        case Nil => Nil
        case head :: tail => tail
      }
    }

    def checkName(name: String): Option[String] = {
      if(name == "clock") {
        Some(name)
      }
      else if(name == "reset") {
        Some(name)
      }
      else if(name.startsWith(varPrefix)) {
        if(newVarPrefix.nonEmpty) {
          Some(newVarPrefix + name.drop(varPrefix.length))
        }
        else {
          Some(name)
        }
      }
      else {
        None
      }
    }

    def addVar(s: String): Unit = {
      s match {
        case VarSpec("wire", sizeString, idString, referenceString) =>
          checkName(referenceString.split(" +").head) match {
            case Some(varName) =>
              if(desiredScopeFound) {
                val wire: Wire = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
                if(! wires.contains(idString)) {
                  wires(idString) = wire
                  logger.debug(s"AddVar $wire at line ${words.currentLineNumber}")
                  currentScope.foreach(_.wires += wire)
                } else {
                  logger.debug(
                    s"AddVar aliased wire $wire at line ${words.currentLineNumber}")
                  aliasedWires(idString) += wire
                  currentScope.foreach(_.wires += wire)
                }
              }
              else {
                logger.debug(s"Ignore var ${scopePathString(currentScope)}$varName at line ${words.currentLineNumber}")
                skippedWires(idString) = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
              }
            case _ =>
              val varName = referenceString.split(" +").head
              logger.debug(s"Ignore var ${scopePathString(currentScope)}$varName at line ${words.currentLineNumber}")
              skippedWires(idString) = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
          }
        case _ =>
          logger.warn(s"Could not parse var $s at line ${words.currentLineNumber}")
      }
    }

    def processVar(): Unit = {
      if(words.hasNext) {
        words.next match {
          case EndSection() =>
            addVar(currentVar.toString())
            currentVar.clear()
          case text =>
            currentVar.append(s" $text")
            processVar()
        }
      }
    }

    def processHeader(builder: StringBuilder): Unit = {
      if(words.hasNext) {
        if(words.next match {
          case EndSection() =>
            false
          case text =>
            builder.append(s" $text")
            true
        }) {
          processHeader(builder)
        }
      }
    }

    def processDump(): Unit = {
      valuesAtTime(-1L)  // this initializes a primordial -1 time slot for initialization
      while(words.hasNext) {
        val nextWord = words.next
        // logger.debug(s"Process dump $nextWord at line ${words.currentLineNumber}")
        nextWord match {
          case ValueChangeScalar(value, varCode) =>
            if(wires.contains(varCode)) {
              logger.debug(s"Change scalar ${wires(varCode)} ${BigInt(value)} at line ${words.currentLineNumber}")
              valuesAtTime(currentTime) += Change(wires(varCode), BigInt(value))
            }
            else {
              logger.warn(
                s"Found change value for $varCode but this key not defined  at line ${words.currentLineNumber}")
            }
          case ValueChangeVector(radixString, value) =>
            if(words.hasNext) {
              val varCode = words.next
              if(wires.contains(varCode)) {
                supportedVectorRadix.get(radixString) match {
                  case Some(radix) =>
                    valuesAtTime(currentTime) += Change(wires(varCode), BigInt(value, radix))
                  case None =>
                    logger.warn(
                      s"Found change value for $varCode but " +
                        s"radix $radixString not supported at line ${words.currentLineNumber}")
                }
              }
            }
          case TimeStamp(timeValue) =>
            currentTime = timeValue.toLong
            logger.debug(s"current time now $currentTime at line ${words.currentLineNumber}")
          case EndSection() =>
            logger.debug(s"end of dump at line ${words.currentLineNumber}")
          case text =>
        }
      }
    }

    def processSections(): Unit = {
      if (words.hasNext) {
        val nextWord = words.next
        nextWord match {
          case SectionHeader(sectionHeader) =>
            logger.debug(s"processing section header $sectionHeader at line ${words.currentLineNumber}")
            sectionHeader match {
              case "date" => processHeader(dateHeader)
              case "version" => processHeader(versionHeader)
              case "comment" => processHeader(commentHeader)
              case "timescale" => processHeader(timeScaleHeader)
              case "scope" => processScope()
              case "upscope" => processUpScope()
              case "var" => processVar()
              case "enddefinitions" => processHeader(endDefinition)
              case "dumpvars" => processDump()
              case _ =>
            }
          case _ =>
            processDump()
            logger.debug("skipping at line ${words.currentLineNumber}")
        }
        processSections()
      }
    }

    // Here is where things get kicked off. You need to parse the various header fields before the VCD can be created
    processSections()

    if(scopeRoot.isEmpty) {
      logger.error(s"Error: No start scope found, desired StartScope is $startScope")
    }

    val vcd = VCD(
      dateHeader.toString().trim, versionHeader.toString().trim,
      commentHeader.toString().trim, timeScaleHeader.toString().trim, "")

    vcd.wires ++= wires
    vcd.valuesAtTime ++= valuesAtTime
    vcd.aliasedWires = aliasedWires
    scopeRoot match {
      case Some(scope) => vcd.scopeRoot = scope
      case None =>
    }
    vcd
  }

  /**
    * This exercises vcd reading and optionally writing
    * and depending up filtering options can pull out only those change values that
    * are specific to a particular module
    * @param args command lines strings use --help to see what they are
    */
  def main(args: Array[String]) {
    val manager = new VCDOptionsManager

    if(manager.parse(args)) {
      val config = manager.vcdConfig

      val vcd = read(
        vcdFile = config.vcdSourceName,
        startScope = config.startScope,
        renameStartScope = config.renameStartScope,
        varPrefix = config.varPrefix,
        newVarPrefix = config.newVarPrefix
      )

      println(s"${vcd.info}")

      if(config.vcdTargetName.nonEmpty) {
        vcd.write(config.vcdTargetName) }
    }
    else {
      manager.parser.showUsageAsError()
    }

  }
}

/**
  * Accumulates changes to wires in a running circuit.  If a wire is changed that it doesn't know about it
  * will add it to the list.  Only actual changed values will be seen in final output.  This version only supports
  * a single top level scope because right now that is what the firrtl-interpreter supports.  It probably is not too
  * too hard to add, all wires are initialized to 'x' in this version.
 *
  * @param date date file was created
  * @param version this software version, but I suppose this could be a DUT version
  * @param comment could be a comment
  * @param timeScale seems to be more text (I like to work in picoseconds)
  * @param scope Not really used here except as the name of the top level module
  */
case class VCD(
           date: String,
           version: String,
           comment: String,
           timeScale: String,
           scope: String

         ) {
  var currentIdNumber = 0
  var timeStamp = -1L
  val lastValues = new mutable.HashMap[String, Change]
  val valuesAtTime = new mutable.HashMap[Long, mutable.HashSet[Change]]
  var scopeRoot = Scope("")
  val wires = new mutable.HashMap[String, Wire]
  var aliasedWires = new mutable.HashMap[String, mutable.HashSet[Wire]] {
    override def default(key: String): mutable.HashSet[Wire] = {
      this(key) = new mutable.HashSet[Wire]
      this(key)
    }
  }

  def info: String = {
    val infoLines = Seq(
      "vcd" -> version,
      "timescale" -> timeScale,
      "comment" -> comment,
      "date" -> date,
      "unique wires" -> wires.size.toString,
      "events" -> valuesAtTime.size.toString
    )
    val maxLabel: Int = infoLines.filter(_._2.trim.nonEmpty).map(_._1.length).max
    infoLines.flatMap { case (label, value) =>
      if(value.trim.nonEmpty) {
        Some(label + ":" + (" " * (4 + maxLabel - label.length)) + value)
      }
      else {
        None
      }
    }.mkString("\n")
  }

  def getIdString(value: Int = currentIdNumber, currentString: String = ""): String = {
    val index = value % VCD.numberOfIdChars
    val newValue = value / VCD.numberOfIdChars

    if(newValue <= 0) {
      VCD.idChars(index) + currentString
    }
    else {
      getIdString(newValue, VCD.idChars(index) + currentString)
    }
  }

  def addWire(wireName: String, width: Int): Unit = {
    val wire = Wire(wireName, getIdString(), width)
    wires(wire.name) = wire
    incrementId()
  }

  def wireChanged(wireName: String, value: BigInt, width: Int = 1): Unit = {

    def updateInfo(): Unit = {
      val wire = wires(wireName)
      val change = Change(wire, value)
      lastValues(wireName) = change
      val changeSet = valuesAtTime.getOrElseUpdate(timeStamp, new mutable.HashSet[Change])
      changeSet += change
    }

    if(! wires.contains(wireName)) {
      addWire(wireName, width: Int)
    }
    lastValues.get(wireName) match {
      case Some(lastValue) =>
        if(lastValue.value != value) {
          updateInfo()
        }
      case _ =>
        updateInfo()
    }
  }

  def incrementTime(increment: Int = 1) {
    timeStamp += 1
  }


  def incrementId(): Unit = currentIdNumber += 1

  def serializeChanges: String = {
    val s = new StringBuilder

    valuesAtTime.keys.toList.sorted.foreach { time =>
      valuesAtTime.get(time).foreach { changeSet =>
        s.append(s"#$time\n")
        changeSet.foreach { change =>
          s.append(change.serialize + "\n")
        }
      }
    }
    s.toString()
  }

  def serializeStartup: String = {
    wires.values.map { wire =>
      val dummyChange = Change(wire, 0)
      dummyChange.serializeUninitialized
    }.mkString("\n")
  }

  def serialize: String = {
    val buffer = new StringBuilder

    buffer.append(VCD.DateDeclaration + "\n")
    buffer.append(date + "\n")
    buffer.append(VCD.End + "\n")
    buffer.append(VCD.VersionDeclaration + "\n")
    buffer.append(version + "\n")
    buffer.append(VCD.End + "\n")
    buffer.append(VCD.CommentDeclaration + "\n")
    buffer.append(comment + "\n")
    buffer.append(VCD.End + "\n")
    buffer.append(s"${VCD.TimeScaleDeclaration} $timeScale  ${VCD.End}\n")

    def doScope(scope: Scope, depth: Int = 0): Unit = {
      def indent(inc: Int = 0): String = " " * ( depth + inc )
      buffer.append(s"${indent(0)}${VCD.ScopeDeclaration} module ${scope.name} ${VCD.End}\n")
      scope.wires.foreach { wire => buffer.append(indent(1) + wire.toString + "\n") }
      scope.subScopes.foreach { subScope => doScope(subScope, depth + 2) }
      buffer.append(s"${indent(0)}${VCD.UpScopeDeclaration} ${VCD.End}\n")
    }

    doScope(scopeRoot)
    buffer.append(s"${VCD.EndDefinitionsDeclaration} ${VCD.End}\n")
    buffer.append(s"${VCD.DumpVarsDeclaration}\n")
    buffer.append(serializeStartup + s"\n${VCD.End}\n")
    buffer.append(serializeChanges)
    buffer.toString()
  }


  def write(fileName: String): Unit = {
    new PrintWriter(fileName) {
      write(serialize)
      close()
    }
  }
}

case class Wire(name: String, id: String, width: Int, path: Array[String] = Array.empty) {
  def fullName: String = (path ++ Seq(name)).mkString(".")
  override def toString: String = {
    s"${VCD.VarDeclaration} wire $width $id $name ${VCD.End}"
  }
}

/**
  * holds the information about
 *
  * @param wire wire who's status is being monitored
  * @param value the value this wire now has
  */
case class Change(wire: Wire, value: BigInt) {
  def serialize: String = {
    if(wire.width == 1) {
      s"$value${wire.id}"
    }
    else {
      "b" +
        (wire.width - 1 to 0 by -1).map { index => if (value.testBit(index)) "1" else "0" }.mkString("") +
        s" ${wire.id}"
    }
  }
  def serializeUninitialized: String = {
    if(wire.width == 1) {
      s"$value${wire.id}"
    }
    else {
      "b" +
        (wire.width - 1 to 0 by -1).map { index => "x" }.mkString("") +
        s" ${wire.id}"
    }
  }
}

case class Scope(name: String, parent: Option[Scope] = None) {
  val subScopes = new ArrayBuffer[Scope]()
  val wires = new ArrayBuffer[Wire]()
}
