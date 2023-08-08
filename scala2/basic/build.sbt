name := "scala-cookbook-scala2-basic"

scalaVersion := "2.13.11"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.26",
  "ch.qos.logback" % "logback-classic" % "1.2.10",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4",
//  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
//  "com.typesafe.scala-logging" %% "scala-logging-api" % "2.1.2",
  "com.typesafe" % "config" % "1.3.3",
  "commons-io" % "commons-io" % "2.6",
  "com.github.markusbernhardt" % "proxy-vole" % "1.0.5",
  "org.scalatest" %% "scalatest" % "3.2.16", //% Test
)
