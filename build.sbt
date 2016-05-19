name := "firrtl-interpreter"

organization := "edu.berkeley.cs"

version := "0.1"

val chiselVersion = System.getProperty("chiselVersion", "3.0")

scalaVersion := "2.11.7"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases"),
  Resolver.sonatypeRepo("public")
)

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "firrtl" % "0.2-BETA-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.4",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1",
  "com.github.scopt" %% "scopt" % "3.4.0"
)

//javaOptions in run ++= Seq(
    //"-Xms2G", "-Xmx4G", "-XX:MaxPermSize=1024M", "-XX:+UseConcMarkSweepGC")

