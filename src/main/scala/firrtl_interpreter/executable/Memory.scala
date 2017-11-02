// See LICENSE for license details.

package firrtl_interpreter.executable

import firrtl.graph.MutableDiGraph
import firrtl_interpreter._
import firrtl.{MemKind, WireKind}
import firrtl.ir.{DefMemory, IntWidth}

object Memory {
  //scalastyle:off method.length
  /**
    * Builds all the symbols and dependencies for the specified memory.
    * Pipelines are constructed as registers with a regular name and
    * a /in name.  Data travels up-index through a pipeline for both
    * read and write pipelines.
    * @param memory              the specified memory
    * @param expandedName        the full name of the memory
    * @param keysDependOnSymbols external graph of dependencies
    * @param symbolsDependOnKeys external graph of dependencies
    * @return
    */
  def buildSymbols(
                    memory: DefMemory,
                    expandedName: String,
                    keysDependOnSymbols: MutableDiGraph[Symbol],
                    symbolsDependOnKeys: MutableDiGraph[Symbol]
  ): Seq[Symbol] = {
    val memorySymbol = Symbol(expandedName, memory.dataType, MemKind, memory.depth)
    val addrWidth = IntWidth(requiredBitsForUInt(memory.depth - 1))

    def buildPipelineDependencies(rootSymbol:      Symbol,
                                  pipelineSymbols: Seq[Symbol],
                                  tailSymbol:      Option[Symbol] = None): Unit = {

      val chain = Seq(rootSymbol) ++ pipelineSymbols ++ (if(tailSymbol.isDefined) Seq(tailSymbol.get) else Seq.empty)

      chain.grouped(2).withFilter(_.length == 2).toList.foreach {
        case source :: target :: Nil =>
          keysDependOnSymbols.addEdge(target, source)
          symbolsDependOnKeys.addEdge(source, target)
        case _ =>
      }
    }

    val readerSymbols = memory.readers.flatMap { readerString =>
      val readerName = s"$expandedName.$readerString"

      val en   = Symbol(s"$readerName.en",   firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk  = Symbol(s"$readerName.clk",  firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr = Symbol(s"$readerName.addr", firrtl.ir.UIntType(addrWidth), WireKind)
      val data = Symbol(s"$readerName.data", memory.dataType, WireKind)

      val readerInterfaceSymbols = Seq(en, clk, addr, data)

      val pipelineDataSymbols = (0 until memory.readLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readerString.pipeline_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readerString.pipeline_$n", memory.dataType, WireKind)
        )
      }

      buildPipelineDependencies(addr, pipelineDataSymbols, Some(data))

      readerInterfaceSymbols ++ pipelineDataSymbols
    }

    val writerSymbols = memory.writers.flatMap { writerString =>
      val writerName = s"$expandedName.$writerString"
      val en    = Symbol(s"$writerName.en", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val clk   = Symbol(s"$writerName.clk", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val addr  = Symbol(s"$writerName.addr", firrtl.ir.UIntType(addrWidth), WireKind)
      val mask  = Symbol(s"$writerName.mask", firrtl.ir.UIntType(IntWidth(1)), WireKind)
      val data  = Symbol(s"$writerName.data", memory.dataType, WireKind)
      val valid = Symbol(s"$writerName.valid", firrtl.ir.UIntType(IntWidth(1)), WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, mask, data, valid)

      val pipelineValidSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$writerString.pipeline_valid_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$writerString.pipeline_valid_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(en, pipelineValidSymbols)

      val pipelineDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$writerString.pipeline_data_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$writerString.pipeline_data_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(data, pipelineDataSymbols)

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$writerString.pipeline_addr_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$writerString.pipeline_addr_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(addr, pipelineAddrSymbols)

      memoryInterfaceSymbols ++ pipelineValidSymbols ++ pipelineAddrSymbols ++ pipelineDataSymbols
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
      val valid = Symbol(s"$writerName.valid", firrtl.ir.UIntType(IntWidth(1)), WireKind)

      val memoryInterfaceSymbols = Seq(en, clk, addr, rdata, mode, mask, wdata, valid)

      val pipelineReadDataSymbols = (0 until memory.readLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_rdata_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_rdata_$n", memory.dataType, WireKind)
        )
      }

      buildPipelineDependencies(addr, pipelineReadDataSymbols, Some(rdata))

      val pipelineEnableSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_valid_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_valid_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(en, pipelineEnableSymbols)

      val pipelineWriteDataSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_wdata_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_wdata_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(wdata, pipelineWriteDataSymbols)

      val pipelineAddrSymbols = (0 until memory.writeLatency).flatMap { n =>
        Seq(
          Symbol(s"$expandedName.$readWriterString.pipeline_addr_$n/in", memory.dataType, WireKind),
          Symbol(s"$expandedName.$readWriterString.pipeline_addr_$n", memory.dataType, WireKind)
        )
      }
      buildPipelineDependencies(addr, pipelineAddrSymbols)

      memoryInterfaceSymbols    ++
        pipelineReadDataSymbols ++
        pipelineEnableSymbols   ++
        pipelineAddrSymbols     ++
        pipelineWriteDataSymbols
    }

    Seq(memorySymbol) ++ readerSymbols ++ writerSymbols ++ readerWriterSymbols
  }
}
