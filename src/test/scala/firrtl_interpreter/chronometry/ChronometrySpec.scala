// See LICENSE for license details.

package firrtl_interpreter.chronometry

import firrtl.WireKind
import firrtl.ir.{IntWidth, SIntType, UIntType}
import org.scalatest.{FreeSpec, Matchers}
import firrtl_interpreter.executable._

// scalastyle:off magic.number
class ChronometrySpec extends FreeSpec with Matchers {
  "UTC can schedule events for single clock" in {
    val utc = new UTC()
    val basicClock = ScheduledClock(Symbol("bob", UIntType(IntWidth(1)), WireKind ), 100)
    utc.register(basicClock)

    for(i <- 0 until 10) {
      val events = utc.nextClocks()
      events.length should be (1)
      println(events.head)
    }
  }

  "UTC can schedule events for two clocks, 1 to 3 ratio" in {
    val utc = new UTC()
    val basicClock = ScheduledClock(Symbol("bob", UIntType(IntWidth(1)), WireKind ), 100)
    val oneThirdClock = ScheduledClock(Symbol("carol", UIntType(IntWidth(1)), WireKind ), 300)
    utc.register(basicClock)
    utc.register(oneThirdClock)

    for(i <- 0 until 10) {
      val events = utc.nextClocks()
      if(i % 3 == 2) {
        events.length should be (2)

      }
      else {
        events.length should be (1)

      }
      println(s"Events at time ${utc.time}")
      println(events.mkString("\n"))
    }
  }
}
