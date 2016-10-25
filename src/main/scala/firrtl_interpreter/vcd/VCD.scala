// See LICENSE for license details.

package firrtl_interpreter.vcd

import java.io.PrintWriter
import java.text.{SimpleDateFormat, DateFormat}

import collection._
import java.util.{Date, TimeZone, Calendar}

object VCD {
  val Version = "0.1"

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
  var timeStamp = 0L
  val lastValues = new mutable.HashMap[String, Change]
  val valuesAtTime = new mutable.HashMap[Long, mutable.HashSet[Change]]
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
    var time = 0L
    val s = new StringBuilder
    while( time <= timeStamp) {
      valuesAtTime.get(time).foreach { changeSet =>
        s.append(s"#$time\n")
        changeSet.foreach { change =>
          s.append(change.serialize + "\n")
        }
      }
      time += 1
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
    val header =
      s"""
        |${VCD.DateDeclaration}
        |  $date
        |${VCD.End}
        |${VCD.VersionDeclaration}
        |  $version
        |${VCD.End}
        |${VCD.CommentDeclaration}
        |  $comment
        |${VCD.End}
        |${VCD.TimeScaleDeclaration} $timeScale  ${VCD.End}
        |${VCD.ScopeDeclaration} module $scope ${VCD.End}""".stripMargin

    val wireSection = wires.keys.toSeq.sorted.map { case key =>
      wires(key).toString
    }.mkString("\n")

    header + "\n" +
      wireSection + "\n" +
      s"${VCD.UpScopeDeclaration} ${VCD.End}\n" +
      s"${VCD.EndDefinitionsDeclaration} ${VCD.End}\n" +
      s"${VCD.DumpVarsDeclaration}\n" +
      serializeStartup + "\n" +
      serializeChanges
  }

  def write(fileName: String): Unit = {
    new PrintWriter(fileName) {
      write(serialize)
      close()
    }
  }
}

case class Wire(name: String, id: String, width: Int) {
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
