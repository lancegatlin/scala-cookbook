import sbt.Keys._
import sbt._

object CleanUpSlf4j {
  // fix multiple slf4j deps
  // see https://stackoverflow.com/questions/25208943/how-to-fix-slf4j-class-path-contains-multiple-slf4j-bindings-at-startup-of-pl
  val settings : Seq[Def.Setting[_]] = Seq(
    libraryDependencies ++= Seq(
      "org.slf4j" % "slf4j-api" % "1.7.26",
      "ch.qos.logback" % "logback-classic" % "1.2.3"
    ).map(_.force()),
    libraryDependencies ~= { _.map(_.exclude("org.slf4j", "slf4j-log4j12")) }
  )

}
