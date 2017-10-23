// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl_interpreter._
import firrtl.{MemKind, WireKind}
import firrtl.ir.{DefMemory, IntWidth}

import scala.collection.mutable

object Memory {
  //scalastyle:off method.length
  def buildSymbols(
                    memory: DefMemory,
                    expandedName: String,
                    dependencies: mutable.HashMap[Symbol, Set[Symbol]]
  ): Seq[Symbol] = {
    val memorySymbol = Symbol(expandedName, memory.dataType, MemKind, memory.depth)
    val addrWidth = IntWidth(requiredBitsForUInt(memory.depth-1))

    val readerSymbols = memory.readers.flatMap { readerString =>
      val readerName = s"$expandedName.$readerString"

      val en   = Symbol(s"$readerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk  = Symbol(s"$readerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr = Symbol(s"$readerName.addr", firrtl.ir.UIntType(addrWidth), WireKind)
      val data = Symbol(s"$readerName.data", memory.dataType, WireKind)
      val readerInterfaceSymbols = Seq(en, clk, addr, data)

      val pipelineDataSymbols = (0 until memory.readLatency).map { n =>
        Symbol(s"$expandedName.$readerString.pipeline_$n", memory.dataType, WireKind)
      }
      val chain = Seq(data) ++ pipelineDataSymbols
      chain.zip(chain.tail).foreach { case (target, source) =>
        dependencies(target) = Set(source)
      }
      readerInterfaceSymbols ++ pipelineDataSymbols
    }

    val writerSymbols = memory.writers.flatMap { writerString =>
      val writerName = s"$expandedName.$writerString"
      val en =  Symbol(s"$writerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk =  Symbol(s"$writerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr =  Symbol(s"$writerName.addr", firrtl.ir.UIntType(addrWidth), WireKind)
      val mask =  Symbol(s"$writerName.mask", memory.dataType, WireKind)
      val data =  Symbol(s"$writerName.data", memory.dataType, WireKind)
      val memoryInterfaceSymbols = Seq(en, clk, addr, mask, data)

      def buildWriteDependencies(rootSymbol: Symbol, pipeLineSymbols: Seq[Symbol]): Unit = {
        val chain = (Seq(rootSymbol) ++ pipeLineSymbols).reverse
        chain.zip(chain.tail).foreach { case (target, source) =>
          dependencies(target) = Set(source)
        }
      }

      val pipeLineEnableSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$writerString.pipeline_en_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(en, pipeLineEnableSymbols)

      val pipeLineDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$writerString.pipeline_data_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(data, pipeLineDataSymbols)

      val pipeLineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$writerString.pipeline_addr_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(addr, pipeLineAddrSymbols)

      memoryInterfaceSymbols ++ pipeLineEnableSymbols ++ pipeLineAddrSymbols ++ pipeLineDataSymbols
    }

    Seq(memorySymbol) ++ readerSymbols ++ writerSymbols
  }
}
