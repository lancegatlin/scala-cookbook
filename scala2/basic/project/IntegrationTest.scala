import sbt.Keys._
import sbt._

object IntegrationTest {
  // Select integration tests that use local running resources
  lazy val IntgLocalTest = config("intg_local") extend Test
  // Select integration test that connect to dev env
  lazy val IntgDevTest = config("intg_dev") extend Test
  // Select general integration tests
  lazy val IntgTest = config("intg") extend Test

  lazy val configs = Seq(IntgLocalTest, IntgDevTest, IntgTest)

  val settings : Seq[Def.Setting[_]] =
    inConfig(IntgDevTest)(Defaults.testTasks) ++
    inConfig(IntgLocalTest)(Defaults.testTasks) ++
    inConfig(IntgTest)(Defaults.testTasks) ++
    Seq(
    // exclude integration tests in Test
    testOptions in Test := Seq(
      Tests.Argument(TestFrameworks.ScalaTest, "-l","IntgTest"),
      Tests.Argument(TestFrameworks.ScalaTest, "-l","IntgLocalTest"),
      Tests.Argument(TestFrameworks.ScalaTest, "-l","IntgDevTest")
    ),
    // only run tagged tests in IntgTest, IntgLocalTest and IntgDevTest
    testOptions in IntgLocalTest := Seq(
      Tests.Argument(TestFrameworks.ScalaTest, "-n","IntgLocalTest")
    ),
    testOptions in IntgDevTest := Seq(
      Tests.Argument(TestFrameworks.ScalaTest, "-n","IntgDevTest")
    ),
    testOptions in IntgTest := Seq(
      Tests.Argument(TestFrameworks.ScalaTest, "-n","IntgTest")
    ),
    // don't run integration tests in parallel since they use external resources
    parallelExecution in IntgTest := false,
    parallelExecution in IntgDevTest := false,
    parallelExecution in IntgLocalTest := false
  )

}
