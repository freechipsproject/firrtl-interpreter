// See LICENSE for license details.

enablePlugins(BuildInfoPlugin)

ChiselProjectDependenciesPlugin.chiselBuildInfoSettings

ChiselProjectDependenciesPlugin.chiselProjectSettings

name := "firrtl-interpreter"

version := "1.1-SNAPSHOT"

crossScalaVersions := Seq("2.11.11", "2.12.3")

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "firrtl" -> "1.1-SNAPSHOT"
)

def chiselVersion(proj: String): String = {
  sys.props.getOrElse(proj + "Version", defaultVersions(proj))
}

// The Chisel projects we're dependendent on.
val chiselDeps = chisel.dependencies(Seq(
    ("edu.berkeley.cs" %% "firrtl" % chiselVersion("firrtl"), "firrtl")
))

val dependentProjects = chiselDeps.projects

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1",
  "org.scalacheck" %% "scalacheck" % "1.13.4",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1"
) ++ chiselDeps.libraries

lazy val firrtl_interpreter = (project in file("."))
  .dependsOn(dependentProjects.map(classpathDependency(_)):_*)
