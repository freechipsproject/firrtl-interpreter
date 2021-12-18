// SPDX-License-Identifier: Apache-2.0

package firrtl.stage

import firrtl.stage._

import firrtl.{AnnotationSeq, EmitAllModulesAnnotation, EmitCircuitAnnotation, Emitter, FirrtlExecutionResult, Parser}
import firrtl.annotations.NoTargetAnnotation
import firrtl.FileUtils
import firrtl.proto.FromProto
import firrtl.options.{InputAnnotationFileAnnotation, OptionsException, Phase, StageOptions, StageUtils}
import firrtl.options.Viewer
import firrtl.options.Dependency

import scopt.OptionParser

import java.io.File

package object phases {

  implicit class DriverCompatibilityExtensions(x: DriverCompatibility.type) {

    /** Convert an [[firrtl.AnnotationSeq AnnotationSeq]] to a ''deprecated'' [[firrtl.FirrtlExecutionResult
      * FirrtlExecutionResult]].
      * @param annotations a sequence of [[firrtl.annotations.Annotation Annotation]]
      */
    @deprecated("FirrtlExecutionResult is deprecated as part of the Stage/Phase refactor. Migrate to FirrtlStage.", "1.2")
    def firrtlResultView(annotations: AnnotationSeq): FirrtlExecutionResult =
      Viewer[FirrtlExecutionResult].view(annotations)

  }
}
