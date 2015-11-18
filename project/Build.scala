package psp
package build

import scala.Predef.{ conforms => _ }
import sbt._, Keys._, psp.libsbt._, Deps._
import psp.std._
import com.typesafe.sbt.JavaVersionCheckPlugin.autoImport._

object Build extends sbt.Build {
  def consoleDependencies = List(jsr305, ammonite)
  def optimizeArgs        = wordSeq("-optimise -Yinline-warnings")
  def stdArgs             = wordSeq("-Yno-predef -Yno-adapted-args -Yno-imports -unchecked")
  def testDependencies    = Def setting Seq(Deps.scalaReflect.value, scalacheck.copy(configurations = None))

  lazy val api = project setup "psp's non-standard api"
  lazy val std = project setup "psp's non-standard standard library" dependsOn (api, dmz) also spire

  /***
   *** Everything below this line is to navigate the maze that is sbt.
   ***/

  def ammonite               = "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.4.8"
  def classpathDeps          = convertSeq(subprojects): List[ClasspathDep[ProjectReference]]
  def consoleClasspathFiles  = fullClasspath in Compile in "consoleOnly" map (_.files)
  def consoleClasspathString = consoleClasspathFiles map (_ mkString ":")
  def forkConfig             = ForkConfig("psp.ReplMain", ImmutableProperties.empty, sciSeq("-usejavacp"), stdForkOptions)
  def projectRefs            = convertSeq(subprojects): List[ProjectReference]
  def slog                   = settingsLogger
  def stdForkOptions         = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true)
  def subprojects            = List[sbt.Project](api, dmz, std)

  def loudLog(msg: String)(f: String => Unit): Unit           = Seq("", "<*> " + msg, "") foreach f
  def forkRepl: TaskOf[ForkConfig]                            = Def task (forkConfig addJvmOptions ("-cp", consoleClasspathString.value))
  def asInputTask(task: TaskOf[ForkConfig]): InputTaskOf[Int] = Def inputTask task.value(spaceDelimited("<arg>").parsed: _*)
  def asTask(task: TaskOf[ForkConfig]): TaskOf[Int]           = asInputTask(task).toTask("")

  implicit class ProjectOps(val p: Project) {
    def aggregatesAll = p aggregate (projectRefs: _*)
    def allSources    = Def task (sources in Test in p).value ++ (sources in Compile in p).value
    def usesCompiler  = p settings (libraryDependencies += Deps.scalaCompiler.value)
    def usesReflect   = p settings (libraryDependencies += Deps.scalaReflect.value)
    def usesAmmonite  = p settings (console in Compile := asTask(forkRepl).value)

    def setup(): Project             = p.alsoToolsJar also commonSettings(p) also (name := "psp-" + p.id)
    def setup(text: String): Project = setup() also (description := text)
    def hidden(): Project            = p in file(s"./project/${p.id}")
    def helper(): Project            = p.hidden.noArtifacts setup s"helper project ${p.id}" dependsOn (classpathDeps: _*)
  }

  private def commonSettings(p: Project) = standardSettings ++ Seq(
                               resolvers +=  Resolver.mavenLocal,
                                 version :=  sbtBuildProps.buildVersion,
                            scalaVersion :=  scalaVersionLatest,
                      crossScalaVersions :=  Seq(scalaVersion.value),
                                licenses :=  pspLicenses,
                            organization :=  pspOrg,
                           scalacOptions ++= scalacOptionsFor(scalaBinaryVersion.value) ++ stdArgs,
                               maxErrors :=  15,
                        triggeredMessage :=  Watched.clearWhenTriggered,
                       publishMavenStyle :=  true,
   javaVersionPrefix in javaVersionCheck :=  Some("1.7")
  ) ++ (
    if (p.id == "root") Nil
    else Seq(target <<= javaCrossTarget(p.id))
  )

  def aggregateIn[A](key: TaskKey[A], project: Project) = {
    def label = key.key.label
    def nameOf(r: Reference): String = r match {
      case LocalProject(id) => id
      case _                => "" + r
    }
    val aggregated = (project: ProjectDefinition[ProjectReference]).aggregate map nameOf mkString ", "

    Command.command(label) { state =>
      loudLog(show"$label is aggregated in [ $aggregated ]")(s => state.log.info(s))
      state runAll (key in project)
    }
  }

  lazy val root = project.root.setup dependsOn (classpathDeps: _*) settings (
    initialize := { this.settingsLogger = (sLog in GlobalScope).value ; initialize.value },
    commands ++= Seq(
      aggregateIn(clean, compileOnly),
      aggregateIn(compile in Compile, compileOnly),
      aggregateIn(test, testOnly),
      aggregateIn(key.packageTask, publishOnly),
      aggregateIn(publish, publishOnly),
      aggregateIn(publishLocal, publishOnly),
      aggregateIn(publishM2, publishOnly)
    ),
    console in Compile <<=  console in Compile in consoleOnly,
       console in Test <<=  console in Test in consoleOnly,
          watchSources <++= testOnly.allSources,
          watchSources <++= consoleOnly.allSources
  )

  lazy val dmz         = project.hidden setup "psp's non-standard dmz"
  lazy val consoleOnly = project.helper.usesCompiler.usesAmmonite dependsOn (testOnly % "test->test") deps (consoleDependencies: _*)
  lazy val publishOnly = project.helper.aggregatesAll
  lazy val compileOnly = project.helper.aggregatesAll
  lazy val testOnly    = project.helper.aggregatesAll settings (
          testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test  :=  false,
                  logBuffered  :=  false,
          libraryDependencies <++= testDependencies,
                         test  :=  (run in Test toTask "").value
  )

  // Assign settings logger here during initialization so it won't be garbage collected.
  // sbt otherwise will throw an exception if it is used after project loading.
  private[this] var settingsLogger: Logger = _
}
