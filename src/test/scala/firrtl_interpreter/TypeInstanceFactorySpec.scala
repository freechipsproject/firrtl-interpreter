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

import firrtl.ir._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class TypeInstanceFactorySpec extends AnyFlatSpec with Matchers {
  behavior of "TypeInstanceFactory"

  it should "concrete types are initialized with value zero" in {
    for(width <- 0 to 100) {
      val ui = TypeInstanceFactory(UIntType(IntWidth(width)))
      ui.poisoned should be (true)
      ui.width should be (width)

      val si = TypeInstanceFactory(SIntType(IntWidth(width)))
      si.poisoned should be (true)
      si.width should be (width)
    }
  }
  it should "throw exception on other types" in {
    intercept[InterpreterException] {
      TypeInstanceFactory(BundleType(Seq()))
    }
    intercept[InterpreterException] {
      TypeInstanceFactory(VectorType(UIntType(IntWidth(10)), 10))
    }
    intercept[InterpreterException] {
      TypeInstanceFactory(UnknownType)
    }
  }
}
