import sbt.*
import sbt.Keys.*

import scala.util.chaining.scalaUtilChainingOps


case class TestGroupCompositeProject(
  id: String,
  projects: Seq[Project],
  skipTasks: Seq[TaskKey[_]] = Seq(publish, publishLocal, sbt.Keys.`package`)
) extends CompositeProject {
  private def applySkipTasksSettings(project: Project): Project =
    project.settings(
      skipTasks.map { taskKey =>
        taskKey / skip := true
      }:_*
    )

  /**
   * The auto generated test projects for each project (e.g. "${p.id}-it", "${p.id}-ft", etc) ensuring they depend on
   * their respective base project.
   */
  override val componentProjects: Seq[Project] = projects.map { p =>
    Project(id = s"${p.id}-$id", base = file(s"${p.id}/$id"))
      .dependsOn(p)
      .settings(
        // for auto generated test projects, also compile tests after compile command
        (Test / compile) := ((Test / compile) triggeredBy (Compile / compile)).value
      )
      .pipe(applySkipTasksSettings)

  }

  /**
   * The project that aggregates all test projects in this test group (e.g. "it", "ft", etc).
   *
   * Note: because nothing is actually in the project folder itself (e.g. "./it", "./ft"), this project
   * is a hidden folder that only serves to aggregate the test projects and doesn't need to be checked into
   * version control.
   */
  val testGroupProject: Project =
    Project(id = id, base = file(s".$id"))
      .aggregate(componentProjects.map(p => LocalProject(p.id)):_*)
      .pipe(applySkipTasksSettings)
}

