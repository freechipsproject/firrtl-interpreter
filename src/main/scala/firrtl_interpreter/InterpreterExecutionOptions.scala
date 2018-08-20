// See LICENSE for license details.

package firrtl_interpreter

import firrtl.AnnotationSeq
import firrtl.options.OptionsView

case class InterpreterExecutionOptions(
  writeVCD:          Boolean              = false,
  vcdShowUnderscored:Boolean              = false,
  setVerbose:        Boolean              = false,
  setOrderedExec:    Boolean              = false,
  allowCycles:       Boolean              = false,
  randomSeed:        Long                 = System.currentTimeMillis(),
  blackBoxFactories: Seq[BlackBoxFactory] = Seq.empty,
  maxExecutionDepth: Long                 = ExpressionExecutionStack.defaultMaxExecutionDepth,
  showFirrtlAtLoad:  Boolean              = false,
  lowCompileAtLoad:  Boolean              = true
)

//scalastyle:off cyclomatic.complexity
object InterpreterViewer {
  implicit object InterpreterOptionsView extends OptionsView[InterpreterExecutionOptions] {
    def view(options: AnnotationSeq): Option[InterpreterExecutionOptions] = {
      val executionOptions = options.foldLeft(InterpreterExecutionOptions()) { (previousOptions, annotation) =>
        annotation match {
          case WriteVcdAnnotation                    => previousOptions.copy(writeVCD = true)
          case VcdShowUnderScoredAnnotation          => previousOptions.copy(vcdShowUnderscored = true)
          case VerboseAnnotation                     => previousOptions.copy(setVerbose = true)
          case AllowCyclesAnnotation                 => previousOptions.copy(allowCycles = true)
          case RandomSeedAnnotation(seed)            => previousOptions.copy(randomSeed = seed)
          case ShowFirrtlAtLoadAnnotation            => previousOptions.copy(showFirrtlAtLoad = true)
          case DontRunLoweringCompilerLoadAnnotation => previousOptions.copy(lowCompileAtLoad = true)
          case InterpreterBlackBoxFactoriesAnnotation(seq)      =>
            previousOptions.copy(blackBoxFactories = previousOptions.blackBoxFactories ++ seq)
          case _ => previousOptions
        }

      }
      Some(executionOptions)
    }
  }
}
