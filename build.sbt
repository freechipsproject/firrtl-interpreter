// See LICENSE for license details.

enablePlugins(BuildInfoPlugin)

ChiselProjectDependenciesPlugin.chiselBuildInfoSettings

ChiselProjectDependenciesPlugin.chiselProjectSettings

name := "firrtl-interpreter"

version := "1.1-SNAPSHOT"

// The Chisel projects we're dependendent on.
val chiselDeps = chisel.dependencies(Seq(
    ("edu.berkeley.cs" %% "firrtl" % "1.1-SNAPSHOT", "firrtl")
))

val dependentProjects = chiselDeps.projects

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1",
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1"
) ++ chiselDeps.libraries

lazy val firrtl_interpreter = (project in file("."))
  .dependsOn(dependentProjects.map(classpathDependency(_)):_*)
