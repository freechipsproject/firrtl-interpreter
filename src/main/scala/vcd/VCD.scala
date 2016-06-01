// See LICENSE for license details.

package vcd

import java.text.{SimpleDateFormat, DateFormat}

import collection._
import java.util.{Date, TimeZone, Calendar}

class VCDFactory {
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
}

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

  def wireChanged(wireName: String, value: BigInt): Unit = {

    def updateInfo(): Unit = {
      val wire = wires(wireName)
      val change = Change(wire, value)
      lastValues(wireName) = change
      val changeSet = valuesAtTime.getOrElseUpdate(timeStamp, new mutable.HashSet[Change])
      changeSet += change
    }

    lastValues.get(wireName) match {
      case Some(lastValue) =>
        if(lastValue.newValue != value) {
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
      dummyChange.serializeUnitialized
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
}

case class Wire(name: String, id: String, width: Int) {
  override def toString: String = {
    s"${VCD.VarDeclaration} wire $width $id $name ${VCD.End}"
  }
}

case class Change(wire: Wire, newValue: BigInt) {
  def serialize: String = {
    if(wire.width == 1) {
      s"$newValue${wire.id}"
    }
    else {
      "b" +
        (wire.width - 1 to 0 by -1).map { index => if(newValue.testBit(index)) "1" else "0" }.mkString("") +
        s" ${wire.id}"
    }
  }
  def serializeUnitialized: String = {
    if(wire.width == 1) {
      s"$newValue${wire.id}"
    }
    else {
      "b" +
        (wire.width - 1 to 0 by -1).map { index => "x" }.mkString("") +
        s" ${wire.id}"
    }
  }
}
