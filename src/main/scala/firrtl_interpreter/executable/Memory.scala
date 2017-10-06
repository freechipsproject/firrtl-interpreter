// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter._
import firrtl.{MemKind, WireKind}
import firrtl.ir.{DefMemory, IntWidth}

object Memory {
  def buildSymbols(memory: DefMemory, expandedName: String): Seq[Symbol] = {
    val memorySymbol = Symbol(expandedName, memory.dataType, MemKind, memory.depth)
    val addrWidth = IntWidth(requiredBitsForUInt(memory.depth-1))

    val readerSymbols = memory.readers.flatMap { readerString =>
      val readerName = s"$expandedName.$readerString"
      Seq(
        Symbol(s"$readerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind),
        Symbol(s"$readerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind),
        Symbol(s"$readerName.addr", firrtl.ir.UIntType(addrWidth), WireKind),
        Symbol(s"$readerName.data", memory.dataType, WireKind)
      ) ++ (0 until memory.readLatency).map { n =>
        Symbol(s"$expandedName.$readerString.pipeline_$n", memory.dataType, WireKind)
      }
    }

    val writerSymbols = memory.writers.flatMap { writerString =>
      val writerName = s"$expandedName.$writerString"
      Seq(
        Symbol(s"$writerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind),
        Symbol(s"$writerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind),
        Symbol(s"$writerName.addr", firrtl.ir.UIntType(addrWidth), WireKind),
        Symbol(s"$writerName.mask", memory.dataType, WireKind),
        Symbol(s"$writerName.data", memory.dataType, WireKind)
      ) ++ (0 until memory.readLatency).map { n =>
        Symbol(s"$expandedName.$writerString.pipeline_$n", memory.dataType, WireKind)
      }
    }

    Seq(memorySymbol) ++ readerSymbols ++ writerSymbols
  }

  def wireInternals(memory: DefMemory, expandedName: String, scheduler: Scheduler): Unit = {
    //TODO (chick) build this out
    ???
  }
}
