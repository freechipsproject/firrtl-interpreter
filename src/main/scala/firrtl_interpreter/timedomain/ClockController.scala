// See LICENSE for license details.

package firrtl_interpreter.timedomain

import scala.collection.mutable

class ClockController {
  var realTime: Long = 0L

  val events: mutable.ArrayBuffer[Event] = new mutable.ArrayBuffer[Event]()

  def add(event: Event): Unit = {
    events += event
  }



}
