name := "scala-cookbook-scala2-basic"

scalaVersion := "2.13.11"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "2.0.5",
  "ch.qos.logback" % "logback-classic" % "1.4.7",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
//  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
//  "com.typesafe.scala-logging" %% "scala-logging-api" % "2.1.2",
  "com.typesafe" % "config" % "1.4.2",
  "commons-io" % "commons-io" % "2.6",
  "com.github.markusbernhardt" % "proxy-vole" % "1.0.5",
  "org.scalatest" %% "scalatest" % "3.2.16", //% Test
)
