// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.ir.IntWidth
import org.scalatest.{FreeSpec, Matchers}

//scalastyle:off magic.number
class SymbolTableSpec extends FreeSpec with Matchers {
  "Symbol table contains all information about anything with a value in a firrtl circuit" - {
    "Symbol table starts out with no entries" in {
      val s = new SymbolTable()
      s.size should be (0)
    }
    "Symbol table can accept Int entries" in {
      val s = new SymbolTable()
      s.size should be (0)
      s.addSymbol(Symbol("int1", firrtl.ir.SIntType(IntWidth(22))))
      s.size should be (1)
      s.addSymbol(Symbol("int2", firrtl.ir.SIntType(IntWidth(22))))
      s.size should be (2)
      s.getSizes should be ((2, 0, 0))
    }
    "Symbol table can accept BigInt entries" in {
      val s = new SymbolTable()
      s.size should be (0)
      s.addSymbol(Symbol("bigInt1", firrtl.ir.SIntType(IntWidth(99))))
      s.size should be (1)
      s.addSymbol(Symbol("bigInt2", firrtl.ir.SIntType(IntWidth(222))))
      s.size should be (2)
      s.getSizes should be ((0, 0, 2))
    }
  }
}
