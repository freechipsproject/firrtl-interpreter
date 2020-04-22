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

import scala.collection.mutable
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

// scalastyle:off magic.number
class CircuitStateSpec extends AnyFlatSpec with Matchers {
  behavior of "CircuitState"

  it should "be creatable" in {
    val u1Type = UIntType(IntWidth(1))
    val u1Instance = TypeInstanceFactory(u1Type)
    val port0 = Port(NoInfo, "port0", Input, u1Type)
    val port1 = Port(NoInfo, "port1", Output, u1Type)
    val c = new CircuitState(
      inputPorts  = mutable.Map(port0.name -> u1Instance),
      outputPorts = mutable.Map(port1.name -> u1Instance),
      registers   = mutable.Map("reg1" -> u1Instance, "reg2" -> u1Instance),
      memories    = mutable.Map(),
      validNames  = mutable.HashSet("wire0")
    )

    c.inputPorts.size should be (1)
    c.outputPorts.size should be (1)
    c.outputPorts.size should be (1)
    c.registers.size   should be (2)
    c.memories.size    should be (0)
  }

  it should "become stale when a value is set" in {
    val u1Type = UIntType(IntWidth(1))
    val u1Instance = TypeInstanceFactory(u1Type)
    val port0 = Port(NoInfo, "port0", Input, u1Type)
    val port1 = Port(NoInfo, "port1", Output, u1Type)
    val c = new CircuitState(
      inputPorts  = mutable.Map(port0.name -> u1Instance),
      outputPorts = mutable.Map(port1.name -> u1Instance),
      registers   = mutable.Map("reg1" -> u1Instance, "reg2" -> u1Instance),
      memories    = mutable.Map(),
      validNames  = mutable.HashSet("wire0")
    )

    c.isStale = false
    c.setValue("port0", ConcreteUInt(1, 1))
    c.isStale should be (true)
  }

  it should "clear registers and ephemera when cycle is called" in {
    val u1Type = UIntType(IntWidth(1))
    val u1Instance = TypeInstanceFactory(u1Type)
    val port0 = Port(NoInfo, "port0", Input, u1Type)
    val port1 = Port(NoInfo, "port1", Output, u1Type)
    val c = new CircuitState(
      inputPorts  = mutable.Map(port0.name -> u1Instance),
      outputPorts = mutable.Map(port1.name -> u1Instance),
      registers   = mutable.Map("reg1" -> u1Instance, "reg2" -> u1Instance),
      memories    = mutable.Map(),
      validNames  = mutable.HashSet("wire0")
    )

    c.setValue("reg1",   u1Instance)
    c.setValue("reg2",   u1Instance)
    c.setValue("wire0", u1Instance)
    c.nextRegisters("reg1").poisoned should be (true)
    c.getValue("reg1").get.poisoned should be (true)
    c.getValue("wire0").get.poisoned should be (true)
    c.isStale = false
    c.cycle()
    c.nextRegisters.size should be (2)
    c.ephemera.size should be (1)
  }

  it should "have mutable type instances, distinct in copy" in {
    val u1Type = UIntType(IntWidth(1))
    val u1Instance = TypeInstanceFactory(u1Type)
    val port0 = Port(NoInfo, "port0", Input, u1Type)
    val port1 = Port(NoInfo, "port1", Output, u1Type)
    val c = new CircuitState(
      inputPorts  = mutable.Map(port0.name -> u1Instance),
      outputPorts = mutable.Map(port1.name -> u1Instance),
      registers   = mutable.Map("reg1" -> u1Instance, "reg2" -> u1Instance),
      memories    = mutable.Map(),
      validNames  = mutable.HashSet("wire0")
    )

    c.inputPorts(port0.name) = ConcreteUInt(5, 4)

    c.inputPorts(port0.name).value should be (5)
  }
}
