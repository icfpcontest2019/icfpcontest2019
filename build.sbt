
ThisBuild / name := "icfpcontest-2019"
ThisBuild / organization := "lambda"
ThisBuild / scalaVersion := "2.11.12"
ThisBuild / version := "1.0.0"

/** ******************************************/
//           Project structure              //
/** ******************************************/

lazy val contest = project
  .in(file("."))
  .aggregate(
    core,
    infra,
    graphics
  )
  .settings(settings)
  .dependsOn(core, infra)

lazy val core = project
  .in(file("core"))
  .settings(
    name := "core",
    settings,
    libraryDependencies ++= commonDependencies
  )
  .enablePlugins(ScalaJSPlugin)
  .configs(IntegrationTest)
  .settings(Defaults.itSettings: _*)
  .settings(inConfig(IntegrationTest)(ScalaJSPlugin.testConfigSettings): _*)

lazy val infra = project
  .in(file("infra"))
  .settings(
    name := "infra",
    settings,
    fork in Test := true,
    libraryDependencies ++= commonDependencies ++ akka,
    libraryDependencies += geoJSON
  )
  .dependsOn(
    core
  )

lazy val graphics = project
  .in(file("graphics"))
  .settings(
    name := "graphics",
    settings,
    libraryDependencies ++= commonDependencies,
    libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0-M7",
    libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"
  )
  .dependsOn(
    core
  )
  .enablePlugins(ScalaJSPlugin)

/** ******************************************/
//               Dependencies               //
/** ******************************************/


lazy val commonDependencies = Seq(
  parserCombinators,
  scalaIO,
  email,
  commonsIO,
  slick,
  xml,
  math,
  scalaCheck,
  scalaTest,
  specs,
  scopt
)

lazy val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
lazy val geoJSON = "org.locationtech.geotrellis" %% "geotrellis-raster" % "1.1.0"
lazy val scalaIO = "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.3"
lazy val email = "org.apache.commons" % "commons-email" % "1.4"
lazy val commonsIO = "commons-io" % "commons-io" % "2.4"
lazy val slick = "com.typesafe.slick" %% "slick" % "3.3.0"
lazy val xml = "org.scala-lang.modules" %% "scala-xml" % "1.1.1"
lazy val math = "org.apache.commons" % "commons-math3" % "3.6.1"
lazy val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.14.0"
lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5" % "test"
lazy val specs = "org.specs2" %% "specs2-core" % "4.3.4" % "test"
lazy val scopt = "com.github.scopt" %% "scopt" % "3.7.0"

val akka = {
  val akkaV = "2.4.19"
  Seq(
    "com.typesafe.akka" %% "akka-http-spray-json" % "10.0.1",
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV % "test"
  )
}


/** ******************************************/
//               Settings                   //
/** ******************************************/

lazy val settings = commonSettings

lazy val commonSettings = Seq(
  scalacOptions ++= compilerOptions,
  resolvers ++= Seq(
    "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/",
    "Typesafe Snapshots" at "https://repo.typesafe.com/typesafe/snapshots/",
    "Maven repository" at "https://mvnrepository.com/artifact/org/scala-js/scalajs-dom"
  ),
  resolvers ++= Seq(
    "locationtech-releases" at "https://repo.locationtech.org/content/groups/releases",
    "locationtech-snapshots" at "https://repo.locationtech.org/content/groups/snapshots"
  )
)

lazy val compilerOptions =
  Seq("-unchecked", "-deprecation", "-encoding", "utf8")


/** ******************************************/
//               Assembly                   //
/** ******************************************/


mainClass in assembly := Some("lambda.runners.Main")

test in assembly := {}

assemblyJarName in assembly := "icfcontest2019.jar"

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}
