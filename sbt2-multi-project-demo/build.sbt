import scala.util.chaining.scalaUtilChainingOps

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version      := "0.1.0-SNAPSHOT"

ThisBuild / libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.13.0",
  "org.scalatest" %% "scalatest" % "3.2.19" % Test
)

lazy val myapp1: Project = project in file("myapp1")
lazy val myapp2: Project = project in file("myapp2")

lazy val codeProjects: Seq[Project] = Seq(myapp1, myapp2)
lazy val codeProjectRefs: Seq[ProjectReference] = codeProjects.map(p => LocalProject(p.id))

// auto generate test projects for each project, e.g. myapp1-it, myapp2-it
lazy val itTestProjects = TestGroupCompositeProject("it", codeProjects)
// use generated test group project ("it") which aggregates all generated test projects
lazy val it: Project = itTestProjects.testGroupProject
// note: both itGenProjects.testGroupProject & itGenProjects must be assigned to a lazy val in build.sbt for sbt to
// evaluate them correctly. Otherwise, they are created but ignored by sbt at runtime.

lazy val ftTestProjects = TestGroupCompositeProject("ft", codeProjects)
lazy val ft: Project = ftTestProjects.testGroupProject

// auto generate "ut" (unit test) group project aggregating all main project's tests (which should be unit tests only)
// note: this is optional since by default "test" task in each project runs unit tests only
lazy val ut: Project = (project in file(".ut"))
  .aggregate(codeProjectRefs:_*)

lazy val root =
  (project in file("."))
    .aggregate(codeProjectRefs:_*)
    .aggregate(it, ft, ut)
    .settings(
      // skip these for root project itself (still aggregated for subprojects)
      publish / skip := true,
      publishLocal / skip := true,
      sbt.Keys.`package` / skip := true,
      // ensure root project test only runs unit tests from code projects (no it/ft tests)
      Test / test / aggregate := false,
      Test / test := Def.sequential(codeProjects.map(p => (p / Test / test).toTask)).value
    )
