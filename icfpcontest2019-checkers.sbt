ThisBuild / scalaVersion := "2.12.8"
ThisBuild / organization := "org.icfpc"
ThisBuild / name := "Libraries for checking tasks of ICFP Contest 2019"
ThisBuild / version := "1.0.0 -- SNAPSHOT"

val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
val scalaIO = "com.madgag" %% "scala-io-file" % "0.4.9"
val email = "org.apache.commons" % "commons-email" % "1.4"
val commonsIO = "commons-io" % "commons-io" % "2.4"
val slick = "com.typesafe.slick" %% "slick" % "3.3.0"
val xml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
val math = "org.apache.commons" % "commons-math3" % "3.6.1"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"

libraryDependencies ++= Seq (
	parserCombinators,
	scalaIO,
	email,
	commonsIO,
	slick,
	xml,
	math,
	scalaCheck
)

// Akka

/*
val sprayV = "1.3.4"
libraryDependencies += "io.spray" %%  "spray-can"     % sprayV,
libraryDependencies += "io.spray" %%  "spray-routing" % sprayV,
libraryDependencies += "io.spray" %%  "spray-testkit" % sprayV  % "test",
*/


val akka = {
  val akkaV = "2.4.19"
  Seq(
    "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.1",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
  )
}

libraryDependencies ++= akka

// Tests

val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"

val specs = "org.specs2" %% "specs2-core" % "4.3.4" % "test"

libraryDependencies ++= Seq( 
	scalaTest,
	specs 
)

// Miscellanea

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

Revolver.settings


