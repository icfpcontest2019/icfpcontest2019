ThisBuild / name := "icfpcontest-2019"
ThisBuild / organization := "lambda"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / version := "1.0.0 -- SNAPSHOT"


/********************************************/
//           Project structure              //
/********************************************/

lazy val global = project
  .in(file("."))
  .settings(settings)
  .aggregate(
    checkers,
    contest,
    graphics
  )

lazy val checkers = project
  .settings(
    name := "checkers",
    settings,
    libraryDependencies ++= commonDependencies ++ akka
  )


lazy val contest = project
  .settings(
    name := "chain",
    settings,
    libraryDependencies ++= commonDependencies ++ akka
  )
  .dependsOn(
    checkers
  )

lazy val graphics = project
  .settings(
    name := "graphics",
    settings,
    libraryDependencies ++= commonDependencies
  )
  .dependsOn(
    checkers
  )

/********************************************/
//               Dependencies               //
/********************************************/


lazy val commonDependencies = Seq (
	parserCombinators,
	scalaIO,
	email,
	commonsIO,
	slick,
	xml,
	math,
	scalaCheck,
	scalaTest,
	specs 
)

val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
val scalaIO = "com.madgag" %% "scala-io-file" % "0.4.9"
val email = "org.apache.commons" % "commons-email" % "1.4"
val commonsIO = "commons-io" % "commons-io" % "2.4"
val slick = "com.typesafe.slick" %% "slick" % "3.3.0"
val xml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
val math = "org.apache.commons" % "commons-math3" % "3.6.1"
val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"
val specs = "org.specs2" %% "specs2-core" % "4.3.4" % "test"
val akka = {
  val akkaV = "2.4.19"
  Seq(
    "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.1",
    "com.typesafe.akka"   %%  "akka-actor"    % akkaV,
    "com.typesafe.akka"   %%  "akka-testkit"  % akkaV   % "test",
  )}


/********************************************/
//               Settings                   //
/********************************************/

lazy val settings = commonSettings

lazy val commonSettings = Seq (
	scalacOptions ++= compilerOptions,
	resolvers ++= Seq(
	  	"Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
		"Typesafe Snapshots" at "https://repo.typesafe.com/typesafe/snapshots/",
		)
	)

lazy val compilerOptions =
	Seq("-unchecked", "-deprecation", "-encoding", "utf8")


/********************************************/
//               Plugins                    //
/********************************************/

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.7.2") 
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")

