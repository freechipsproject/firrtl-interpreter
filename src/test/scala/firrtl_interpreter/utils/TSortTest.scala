// See LICENSE for license details.

package firrtl_interpreter.utils

import org.scalatest.FreeSpec

class TSortTest extends FreeSpec {

  "Methods tests" - {
    "should find no loops here" in {
      val graph = Map(
        "a" -> Set("b", "c", "d"),
        "c" -> Set("d")
      )
      println(TSort.findLoops(graph).mkString("\n"))
    }
    "should find 1 loops here" in {
      val graph = Map(
        "a" -> Set("b", "c", "d"),
        "c" -> Set("d"),
        "d" -> Set("e"),
        "d" -> Set("e"),
        "e" -> Set("f"),
        "f" -> Set("a")
      )
      println(TSort.findLoops(graph).mkString("\n"))
    }

    "apply" in {

    }

  }
}
