name := "scala-cookbook-scala2-db"

scalaVersion := "2.11.11"

val sparkVersion = "2.2.1"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.26",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
  "com.typesafe.scala-logging" %% "scala-logging-api" % "2.1.2",
  "com.couchbase.client" % "java-client" % "2.7.4",
  "commons-io" % "commons-io" % "2.6",
  "com.zaxxer" % "HikariCP" % "3.3.1",
  "com.github.markusbernhardt" % "proxy-vole" % "1.0.5",
  "org.scalatest" %% "scalatest" % "3.0.5", //% Test
)
