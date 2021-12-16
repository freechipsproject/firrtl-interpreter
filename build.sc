// SPDX-License-Identifier: Apache-2.0

// Build script for mill 0.6.0
import mill._
import mill.scalalib._
import mill.scalalib.publish._
import coursier.maven.MavenRepository
import $ivy.`com.lihaoyi::mill-contrib-buildinfo:$MILL_VERSION`
import mill.contrib.buildinfo.BuildInfo

object firrtlInterpreter extends mill.Cross[firrtlInterpreterCrossModule]("2.13.4", "2.12.12")

// The following stanza is searched for and used when preparing releases.
// Please retain it.
// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map(
  "firrtl" -> "1.5-SNAPSHOT"
)

def getVersion(dep: String, org: String = "edu.berkeley.cs") = {
  val version = sys.env.getOrElse(dep + "Version", defaultVersions(dep))
  ivy"$org::$dep:$version"
}

trait CommonModule extends ScalaModule with SbtModule with PublishModule {
  def firrtlModule: Option[PublishModule] = None

  def firrtlIvyDeps = if (firrtlModule.isEmpty)
    Agg(
      getVersion("firrtl")
    )
  else Agg.empty[Dep]

  def moduleDeps = Seq() ++ firrtlModule

  def ivyDeps = super.ivyDeps() ++ firrtlIvyDeps

  def publishVersion = "1.5-SNAPSHOT"

  protected def majorVersion = crossVersion.split('.')(1).toInt

  def crossVersion: String

  def scalaVersion = crossVersion

  def repositories() = super.repositories ++ Seq(
    MavenRepository("https://oss.sonatype.org/content/repositories/snapshots"),
    MavenRepository("https://oss.sonatype.org/content/repositories/releases")
  )

  private def scalacCrossOptions =  Seq()

  private def javacCrossOptions = Seq("-source", "1.8", "-target", "1.8")

  override def scalacOptions = super.scalacOptions() ++ Agg(
    "-deprecation",
    "-feature"
  ) ++ scalacCrossOptions

  override def javacOptions = super.javacOptions() ++ javacCrossOptions

  def pomSettings = PomSettings(
    description = artifactName(),
    organization = "edu.berkeley.cs",
    url = "https://www.chisel-lang.org",
    licenses = Seq(License.`Apache-2.0`),
    versionControl = VersionControl.github("freechipsproject", "firrtl-interpreter"),
    developers = Seq(
      Developer("jackbackrack", "Jonathan Bachrach", "https://eecs.berkeley.edu/~jrb/")
    )
  )
}

class firrtlInterpreterCrossModule(crossVersionValue: String) extends CommonModule with PublishModule with BuildInfo { m =>
  // different scala version shares same sources
  // mill use foo/2.11.12 foo/2.12.11 as millSourcePath by default
  override def millSourcePath = super.millSourcePath / os.up / os.up

  def crossVersion = crossVersionValue

  def mainClass = Some("firrtl_interpreter.FirrtlRepl")

  def ivyDeps = super.ivyDeps() ++ Agg(
    ivy"com.github.scopt::scopt:4.0.1",
    ivy"org.scala-lang.modules:scala-jline:2.12.1",
    ivy"org.json4s::json4s-native:3.6.11"
  )

  object test extends Tests {
    private def ivyCrossDeps = Agg()

    def ivyDeps = Agg(
      ivy"org.scalatest::scalatest:3.2.9",
      ivy"org.scalacheck::scalacheck:1.15.4"
    ) ++ ivyCrossDeps

    def testFrameworks = Seq("org.scalatest.tools.Framework")

    def testOnly(args: String*) = T.command {
      super.runMain("org.scalatest.run", args: _*)
    }
  }

  override def buildInfoPackageName = Some("firrtl_interpreter")

  override def buildInfoMembers: T[Map[String, String]] = T {
    Map(
      "buildInfoPackage" -> artifactName(),
      "version" -> publishVersion(),
      "scalaVersion" -> scalaVersion()
    )
  }

  override def generatedSources = T {
    Seq(generatedBuildInfo()._2)
  }

  // make mill publish sbt compatible package
  def artifactName = "firrtl-interpreter"
}
