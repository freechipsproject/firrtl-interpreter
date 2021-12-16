/*
Copyright 2020 The Regents of the University of California (Regents)

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 */

// sbt-site - sbt-ghpages

enablePlugins(SiteScaladocPlugin)

enablePlugins(GhpagesPlugin)

git.remoteRepo := "git@github.com:freechipsproject/firrtl-interpreter.git"

def scalacOptionsVersion(scalaVersion: String): Seq[String] = Seq()

def javacOptionsVersion(scalaVersion: String): Seq[String] = Seq("-source", "1.8", "-target", "1.8")

name := "firrtl-interpreter"

organization := "edu.berkeley.cs"

version := "1.5-SNAPSHOT"

scalaVersion := "2.12.13"

crossScalaVersions := Seq("2.12.13", "2.13.6")

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("public")
)

// Provide a managed dependency on X if -DXVersion="" is supplied on the command line.
val defaultVersions = Map("firrtl" -> "1.5-SNAPSHOT")

// Ignore dependencies on Berkeley artifacts.
// scala-steward:off
libraryDependencies ++= (Seq("firrtl").map {
  dep: String => "edu.berkeley.cs" %% dep % sys.props.getOrElse(dep + "Version", defaultVersions(dep)) })
// scala-steward:on

libraryDependencies ++= Seq(
  "junit" % "junit" % "4.13" % "test",
  "org.scalatest" %% "scalatest" % "3.2.9" % "test",
  "org.scalacheck" %% "scalacheck" % "1.15.4" % "test",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1"
)

//javaOptions in run ++= Seq(
    //"-Xms2G", "-Xmx4G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")
//)

publishMavenStyle := true

publishArtifact in Test := false
pomIncludeRepository := { x => false }

// Don't add 'scm' elements if we have a git.remoteRepo definition.
pomExtra := (<url>http://chisel.eecs.berkeley.edu/</url>
<licenses>
  <license>
    <name>BSD-style</name>
    <url>http://www.opensource.org/licenses/bsd-license.php</url>
    <distribution>repo</distribution>
  </license>
</licenses>
<developers>
  <developer>
    <id>chick</id>
    <name>Charles Markley</name>
    <url>https://aspire.eecs.berkeley.edu/author/chick/</url>
  </developer>
</developers>)

publishTo := {
  val v = version.value
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) {
    Some("snapshots" at nexus + "content/repositories/snapshots")
  }
  else {
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
  }
}

scalacOptions in Compile in doc ++= Seq(
  "-diagrams",
  "-diagrams-max-classes", "25",
  "-doc-version", version.value,
  "-sourcepath", baseDirectory.value.getAbsolutePath,
  "-doc-source-url", "https://github.com/ucb-bar/chisel-testers/tree/master/€{FILE_PATH}.scala"
) ++ scalacOptionsVersion(scalaVersion.value)

javacOptions ++= javacOptionsVersion(scalaVersion.value)
