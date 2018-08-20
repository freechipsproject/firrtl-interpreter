// See LICENSE for license details.

package firrtl_interpreter

import firrtl.AnnotationSeq
import firrtl.annotations.NoTargetAnnotation
import firrtl.options.{HasScoptOptions, RegisteredLibrary}
import scopt.OptionParser

sealed trait InterpreterOption extends HasScoptOptions

/**
  * Tells treadle to write a vcd file during simulation
  */
case object WriteVcdAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-write-vcd")
    .abbr("fiiwv")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("writes vcd executioin log, filename will be based on top-name")
}

/**
  * Tells treadle to include _T_* and _GEN_* wires in VCD output
  */
case object VcdShowUnderScoredAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-vcd-show-underscored-vars")
    .abbr("fiivsu")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("vcd output by default does not show var that start with underscore, this overrides that")
}

/**
  *  Tells treadle to execute verbosely
  */
case object VerboseAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-verbose")
    .abbr("fiv")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("makes the treadle very verbose")
}

/**
  *  Tells treadle to allow cycles
  */
case object AllowCyclesAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-allow-cycle")
    .abbr("fiac")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("will try to run when firrtl contains combinational loops")
}

/**
  *  Sets the seed for treadle's private random number generator
  */
case class RandomSeedAnnotation(seed: Long = 0L) extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Long]("fint-random-seed")
    .abbr("firc")
    .action( (x, c) => c :+ RandomSeedAnnotation(x) )
    .unbounded()
    .text("will try to run when firrtl contains combinational loops")
}

/**
  *  Tells treadle to show the low firrtl it is starting out with
  */
case object ShowFirrtlAtLoadAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-show-firrtl-at-load")
    .abbr("fisfal")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("show the low firrtl source treadle is using to build simulator")
}

/**
  *  Tells treadle to not run its own lowering pass on firrtl input (not recommended)
  */
case object DontRunLoweringCompilerLoadAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-dont-run-lower-compiler-on-load")
    .abbr("fidrlcol")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("do not run its own lowering pass on firrtl input (not recommended)")
}

/**
  *  Tells treadle to present random value when validIf's condition is off
  */
case object ValidIfIsRandomAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-validif-random")
    .abbr("fivir")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("validIf returns random value when condition is false")
}

/**
  *  Sets the number of rollback buffers in simulator, useful to see why wires have their valies
  */
//scalastyle:off magic.number
case class RollBackBuffersAnnotation(rollbackBufferDepth: Int = 4) extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Int]("fint-rollback-buffers")
    .abbr("firb")
    .action( (x, c) => c :+ RollBackBuffersAnnotation(x) )
    .unbounded()
    .text("number of rollback buffers, 0 is no buffers, default is 4")
}
/**
  *  Sets the seed for treadle's private random number generator
  */
case class ResetNameAnnotation(symbolNames: String = "") extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("fint-reset-name")
    .abbr("firn")
    .action( (x, c) => c :+ ResetNameAnnotation(x) )
    .unbounded()
    .text("name of the default reset signal")
}

/**
  *  Tells treadle to present random value when validIf's condition is off
  */
case object CallResetAtStartupAnnotation extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[Unit]("fint-call-reset-at-startup")
    .abbr("ficras")
    .action( (_, c) => c :+ this )
    .unbounded()
    .text("makes treadle do it's own reset at startup, usually for internal use only")
}

case class InterpreterFirrtlString(firrtl: String = "") extends NoTargetAnnotation with InterpreterOption {
  def addOptions(p: OptionParser[AnnotationSeq]): Unit = p.opt[String]("fint-firrtl-source-string")
    .abbr("firns")
    .action( (x, c) => c :+ InterpreterFirrtlString(x) )
    .unbounded()
    .text("a serialized firrtl circuit, mostly used internally")
}

case class InterpreterBlackBoxFactoriesAnnotation(blackBoxFactories: Seq[BlackBoxFactory]) extends NoTargetAnnotation

class InterpreterLibrary extends RegisteredLibrary {
  val name: String = "treadle"
  override def addOptions(parser: OptionParser[AnnotationSeq]): Unit = {
    val seq: Seq[HasScoptOptions] = Seq(
      WriteVcdAnnotation,
      VcdShowUnderScoredAnnotation,
      VerboseAnnotation,
      AllowCyclesAnnotation,
      RandomSeedAnnotation(),
      ShowFirrtlAtLoadAnnotation,
      DontRunLoweringCompilerLoadAnnotation,
      ValidIfIsRandomAnnotation,
      RollBackBuffersAnnotation(),
      InterpreterFirrtlString()
    )

    seq.foreach(_.addOptions(parser))
  }
}
