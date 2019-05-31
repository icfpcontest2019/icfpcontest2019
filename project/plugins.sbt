/********************************************/
//               Plugins                    //
/********************************************/

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.7.2") 
addSbtPlugin("io.spray" % "sbt-revolver" % "0.9.1")


addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject"      % "0.6.0")
addSbtPlugin("org.portable-scala" % "sbt-scala-native-crossproject" % "0.6.0")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"                   % "0.6.23")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.0.0-M7")
addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.0-M7")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.9")

libraryDependencies += "org.scala-js" %% "scalajs-env-jsdom-nodejs" % "1.0.0-M7"
