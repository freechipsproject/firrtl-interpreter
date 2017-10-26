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

    def buildWriteDependencies(rootSymbol: Symbol, pipelineSymbols: Seq[Symbol]): Unit = {
      val chain = (Seq(rootSymbol) ++ pipelineSymbols).reverse
      chain.zip(chain.tail).foreach { case (target, source) =>
        dependencies(target) = Set(source)
      }
    }

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
      val mask =  Symbol(s"$writerName.mask", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val data =  Symbol(s"$writerName.data", memory.dataType, WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, mask, data)

      val pipelineEnableSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$writerString.pipeline_en_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(en, pipelineEnableSymbols)

      val pipelineDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$writerString.pipeline_data_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(data, pipelineDataSymbols)

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$writerString.pipeline_addr_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(addr, pipelineAddrSymbols)

      memoryInterfaceSymbols ++ pipelineEnableSymbols ++ pipelineAddrSymbols ++ pipelineDataSymbols
    }

    val readerWriterSymbols = memory.readwriters.flatMap { readWriterString =>
      val writerName = s"$expandedName.$readWriterString"
      val en    =  Symbol(s"$writerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk   =  Symbol(s"$writerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr  =  Symbol(s"$writerName.addr", firrtl.ir.UIntType(addrWidth), WireKind)
      val rdata =  Symbol(s"$writerName.rdata", memory.dataType, WireKind)
      val mode  =  Symbol(s"$writerName.wmode", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val mask  =  Symbol(s"$writerName.wmask", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val wdata =  Symbol(s"$writerName.wdata", memory.dataType, WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, rdata, mode, mask, wdata)

      val pipelineReadDataSymbols = (0 until memory.readLatency).map { n =>
        Symbol(s"$expandedName.$readWriterString.pipeline_read_$n", memory.dataType, WireKind)
      }
      val chain = Seq(rdata) ++ pipelineReadDataSymbols
      chain.zip(chain.tail).foreach { case (target, source) =>
        dependencies(target) = Set(source)
      }

      val pipelineEnableSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$readWriterString.pipeline_en_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(en, pipelineEnableSymbols)

      val pipelineWriteDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$readWriterString.pipeline_write_data_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(wdata, pipelineWriteDataSymbols)

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(Symbol(s"$expandedName.$readWriterString.pipeline_write_addr_$n", memory.dataType, WireKind))
      }
      buildWriteDependencies(addr, pipelineAddrSymbols)

      memoryInterfaceSymbols    ++
        pipelineReadDataSymbols ++
        pipelineEnableSymbols   ++
        pipelineAddrSymbols     ++
        pipelineWriteDataSymbols
    }

    Seq(memorySymbol) ++ readerSymbols ++ writerSymbols ++ readerWriterSymbols
  }
}
