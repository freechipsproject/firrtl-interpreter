// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir.IntWidth
import firrtl_interpreter.{FirrtlTerp, HasInterpreterSuite, InterpreterOptionsManager, ToLoFirrtl}
import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number
class SymbolTableSpec extends FreeSpec with Matchers {
  val gcdFirrtl: String =
    s"""
       |circuit GCD :
       |  module GCD :
       |    input clock : Clock
       |    input reset : UInt<1>
       |    input io_in1 : UInt<16>
       |    input io_in2 : UInt<16>
       |    output io_out1 : UInt<16>
       |    output io_out2 : UInt<16>
       |
       |    node a1 = io_in1
       |    node a2 = a1
       |    node a3 = a2
       |    io_out1 <= a3
       |
       |    node b1 = io_in2
       |    node b2 = b1
       |    reg b3 : UInt<16>, clock with :
       |      reset => (UInt<1>("h0"), b3)
       |    b3 <= b2
       |    node b4 = b3
       |    io_out2 <= b4
    """
      .stripMargin

  """SymbolTable creates a table with dependency information""" in {
    val optionsManager = new InterpreterOptionsManager
    val simulator = FirrtlTerp(gcdFirrtl, optionsManager)

    val symbolTable = simulator.symbolTable
    val scheduler   = simulator.scheduler

    val keyToDependent = symbolTable.keyDependsOnSymbols
    val DependentToKey = symbolTable.symbolDependsOnKeys

    keyToDependent.reachableFrom(symbolTable("clock")).size should be (0)

    scheduler.triggeredAssigns.size should be (1)

    symbolTable.registerNames.toList.sorted.foreach { key =>
      val dependents = symbolTable.keyDependsOnSymbols.reachableFrom(symbolTable(key))

      println(s"${key} => ${dependents.map(_.name).mkString(",")}")
    }

    println("All dependencies")
    symbolTable.symbols.toList.sortBy(_.name).foreach { keySymbol =>
      val dependents = symbolTable.keyDependsOnSymbols.reachableFrom(keySymbol)

      println(s"${keySymbol.name} => ${dependents.map(_.name).mkString(",")}")
    }
  }

}
