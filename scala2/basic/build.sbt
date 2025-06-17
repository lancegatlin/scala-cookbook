name := "scala-cookbook-scala2-basic"

scalaVersion := "2.13.11"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "2.0.16",
  "ch.qos.logback" % "logback-classic" % "1.5.11",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
  "org.typelevel" %% "cats-core" % "2.12.0",
  "org.typelevel" %% "cats-effect" % "3.5.4",
  "org.typelevel" %% "log4cats-core" % "2.7.0",
  "org.typelevel" %% "log4cats-slf4j" % "2.7.0",
  "org.apache.commons" % "commons-lang3" % "3.17.0",
//  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
//  "com.typesafe.scala-logging" %% "scala-logging-api" % "2.1.2",
  "com.typesafe" % "config" % "1.4.3",
  "commons-io" % "commons-io" % "2.17.0",
  "com.github.markusbernhardt" % "proxy-vole" % "1.0.5",
  "org.scalatest" %% "scalatest" % "3.2.19", //% Test
  "org.scalacheck" %% "scalacheck" % "1.18.1",
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.18.0",
  "org.apache.pekko" %% "pekko-stream" % "1.0.3",
  // for Gzip
  "org.apache.pekko" %% "pekko-http" % "1.0.1",
  "co.fs2" %% "fs2-core" % "3.12.0",
  "de.lhns" %% "fs2-compress-gzip" % "2.3.0"
)
