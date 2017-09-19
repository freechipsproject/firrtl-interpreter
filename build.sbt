// See LICENSE for license details.

enablePlugins(BuildInfoPlugin)

ChiselProjectDependenciesPlugin.chiselBuildInfoSettings

ChiselProjectDependenciesPlugin.chiselProjectSettings

name := "firrtl-interpreter"

version := "1.1-SNAPSHOT"

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

pomExtra := pomExtra.value ++ (
<scm>
  <url>https://github.com/freechipsproject/firrtl-interpreter.git</url>
  <connection>scm:git:github.com/freechipsproject/firrlt-interpreter.git</connection>
</scm>
<developers>
  <developer>
    <id>chick</id>
    <name>Charles Markley</name>
    <url>https://aspire.eecs.berkeley.edu/author/chick/</url>
  </developer>
</developers>)

scalacOptions in Compile in doc ++= Seq(
  "-diagrams",
  "-diagrams-max-classes", "25",
  "-doc-version", version.value,
  "-sourcepath", baseDirectory.value.getAbsolutePath,
  "-doc-source-url", "https://github.com/ucb-bar/chisel-testers/tree/master/€{FILE_PATH}.scala"
)

lazy val firrtl_interpreter = (project in file("."))
  .dependsOn(dependentProjects.map(classpathDependency(_)):_*)
