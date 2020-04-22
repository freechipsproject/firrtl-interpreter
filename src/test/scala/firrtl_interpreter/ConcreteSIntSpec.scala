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

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class ConcreteSIntSpec extends AnyFlatSpec with Matchers {
  behavior of "constructor"

  it should "throw exception if not enough bits to hold" in {
    intercept[InterpreterException] {
      ConcreteSInt(4, 2)
    }
  }

  it should "become a negative number if high bit of value is "
}
