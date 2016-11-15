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
  val ValueChangeScalar = """\s*(\d+)(\S)\s*""".r
  val ValueChangeVector = """\s*([rb])([0-9\.]+)s*""".r
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

  //scalastyle:off cyclomatic.complexity method.length
  /**
    * Read and parse the specified vcd file, producing a VCD data structure
 *
    * @param vcdFile name of file to parse
    * @return a populated VCD class
    */
  def read(vcdFile: String, startScope: String = ""): VCD = {
    val wordsBuffer = io.Source.fromFile(vcdFile).getLines().mkString(" ")
    val words = wordsBuffer.split("""\s+""").toIterator

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

    val wires = new mutable.HashMap[String, Wire]
    val ignoredWires = new mutable.HashMap[String, Wire]

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
            scopeRoot = Some(Scope(name))
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
                  logger.debug(s"unsupported scope type ${scopeBuffer.toString()}")
                }
                addScope(moduleName)
              case _ =>
                logger.warn(s"unknown scope format ${scopeBuffer.toString()}")
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

    def addVar(s: String): Unit = {
      s match {
        case VarSpec("wire", sizeString, idString, referenceString) =>
          val varName = referenceString.split(" +").head
          if(desiredScopeFound) {
            logger.debug(s"AddVar ${scopePathString(currentScope)}$varName")
            wires(idString) = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
            currentScope.foreach(_.wires += wires(idString))
          }
          else {
            logger.debug(s"Ignore var ${scopePathString(currentScope)}$varName")
            ignoredWires(idString) = Wire(varName, idString, sizeString.toInt, scopePath(currentScope).toArray)
          }
        case _ =>
          logger.warn(s"Could not parse var $s")
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
      valuesAtTime(-1L)
      while(words.hasNext) {
        val nextWord = words.next
        logger.debug(s"Process dump $nextWord")
        nextWord match {
          case ValueChangeScalar(value, varCode) =>
            if(! ignoredWires.contains(varCode)) {
              valuesAtTime(currentTime) += Change(wires(varCode), BigInt(value))
            }
          case ValueChangeVector("b", value) =>
            if(words.hasNext) {
              val varCode = words.next
              if(! ignoredWires.contains(varCode)) {
                valuesAtTime(currentTime) += Change(wires(varCode), BigInt(value, 2))
              }
            }
            else {

            }
          case ValueChangeVector("r", value) =>
            if(words.hasNext) {
              words.next
              logger.warn(s"Error r$value 'r' format not supported")
            }
          case TimeStamp(timeValue) =>
            currentTime = timeValue.toLong
            logger.debug(s"current time now $currentTime")
          case EndSection() =>
            logger.debug(s"end of dump")
          case text =>
        }
      }
    }

    def processSections(): Unit = {
      if (words.hasNext) {
        words.next() match {
          case SectionHeader(sectionHeader) =>
            logger.debug(s"processing section header $sectionHeader")
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
            logger.debug("skipping")
        }
        processSections()
      }
    }

    // Here is where things get kicked off. You need to parse the various header fields before the VCD can be created
    processSections()

    val vcd = VCD(
      dateHeader.toString(), versionHeader.toString(), commentHeader.toString(), timeScaleHeader.toString, "")

    vcd.wires ++= wires
    vcd.valuesAtTime ++= valuesAtTime
    scopeRoot match {
      case Some(scope) => vcd.scopeRoot = scope
      case None =>
    }
    vcd

  }

  def main(args: Array[String]) {
    val vcd = read(args.head)
    println(s"vcd = $vcd")

    vcd.write(args.tail.head)
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
