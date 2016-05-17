name := "firrtl-interpreter"

organization := "edu.berkeley.cs"

version := "0.1"

val chiselVersion = System.getProperty("chiselVersion", "3.0")

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "edu.berkeley.cs" %% "firrtl" % "0.1-SNAPSHOT",
  "org.scalatest" % "scalatest_2.11" % "2.2.4",
  "org.scalacheck" %% "scalacheck" % "1.12.4")

