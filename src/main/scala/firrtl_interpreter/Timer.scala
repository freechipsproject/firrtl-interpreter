// See LICENSE for license details.

package firrtl_interpreter

import scala.StringBuilder
import scala.collection.mutable


case class TimerEvent(tag: String) {
  var events  = 0L
  var nanoseconds = 0L
  var lastEventNanoseconds = 0L
}

object Timer {
  val TenTo9th = 1000000000.0
  var enabled = true
  val timingLog = new mutable.HashMap[String, TimerEvent]

  def apply[R](tag: String)(block: => R): R = {
    if(enabled) {
      val t0 = System.nanoTime()
      val result = block // call-by-name
      val t1 = System.nanoTime()

      val timerEvent = timingLog.getOrElseUpdate(tag, new TimerEvent(tag))
      timerEvent.events += 1
      val delta = t1 - t0
      timerEvent.nanoseconds += delta
      timerEvent.lastEventNanoseconds = delta
      result
    }
    else {
      block
    }
  }

  def entryFor(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) => s"${entry.events}:${entry.nanoseconds}:${entry.nanoseconds / entry.events}"
      case _           => ""
    }
  }

  def prettyEntryForTag(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) =>
        val total_seconds = entry.nanoseconds.toDouble / TenTo9th
        val averageSeconds = (entry.nanoseconds.toDouble / entry.events.toDouble) / TenTo9th
        s"${entry.events}:${entry.nanoseconds}:${}"
      case _           => ""
    }
  }

  def prettyLastTime(tag: String): String = {
    timingLog.get(tag) match {
      case Some(entry) =>
        val lastEventSeconds = entry.lastEventNanoseconds.toDouble / TenTo9th
        s"$lastEventSeconds"
      case _           => ""
    }
  }

  def clear(): Unit = {
    timingLog.clear()
  }

  def report(): String = {
    val sortedTags = timingLog.keys.toSeq.sorted
    sortedTags.map { tag =>
      s"$tag:${entryFor(tag)}"
    }.mkString("\n")
  }
}
