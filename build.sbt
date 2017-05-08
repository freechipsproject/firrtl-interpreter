// See LICENSE for license details.

import chiselBuild.ChiselDependencies._
import chiselBuild.ChiselSettings

ChiselSettings.commonSettings

ChiselSettings.publishSettings

val internalName = "firrtl_interpreter"

name := "firrtl-interpreter"

version := "1.1-SNAPSHOT"

libraryDependencies ++= chiselLibraryDependencies(internalName)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.4",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1",
  "com.github.scopt" %% "scopt" % "3.4.0"
)

//javaOptions in run ++= Seq(
    //"-Xms2G", "-Xmx4G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")

dependsOn((chiselProjectDependencies(internalName)):_*)
