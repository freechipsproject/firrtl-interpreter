/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

package firrtl_interpreter


import scala.collection.mutable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RandomConcreteSpec extends AnyFlatSpec with Matchers {
  behavior of "random sint generator"

  they should "work over all possible values of given width" in {
    var count = 0

    for(width <- 1 to 8) {
      val (low, high) = TestUtils.extremaOfSIntOfWidth(width)
      val range = new mutable.HashSet[BigInt]

      (low to high).foreach { b => range += b }

      while (count < 10000 && range.nonEmpty) {
        val c = Concrete.randomSInt(width)
        // println(s"got rand sint $c")

        range -= c.value
        count += 1
      }

      assert(range.isEmpty, s"range not empty $range")
    }
  }
}
