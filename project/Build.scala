package psp
package build

import scala.Predef.{ conforms => _ }
import sbt._, Keys._, psp.libsbt._, Deps._
import psp.std._
import com.typesafe.sbt.JavaVersionCheckPlugin.autoImport._

object Build extends sbt.Build {
  // Assign settings logger here during initialization so it won't be garbage collected.
  // sbt otherwise will throw an exception if it is used after project loading.
  private[this] var settingsLogger: Logger = _

  def slog          = settingsLogger
  def optimizeArgs  = wordSeq("-optimise -Yinline-warnings")
  def stdArgs       = wordSeq("-Yno-predef -Yno-adapted-args -Yno-imports -unchecked")
  def subprojects   = List[sbt.Project](api, dmz, std)
  def classpathDeps = convertSeq(subprojects): List[ClasspathDep[ProjectReference]]
  def projectRefs   = convertSeq(subprojects): List[ProjectReference]

  implicit class ProjectOps(val p: Project) {
    def allSources                   = Def task (sources in Test in p).value ++ (sources in Compile in p).value
    def setup(): Project             = p.alsoToolsJar also commonSettings(p) also (name := "psp-" + p.id)
    def setup(text: String): Project = setup() also (description := text)
    def usesCompiler                 = p settings (libraryDependencies += Deps.scalaCompiler.value)
    def usesReflect                  = p settings (libraryDependencies += Deps.scalaReflect.value)
    def helper(): Project            = p.noArtifacts setup "helper project" dependsOn (classpathDeps: _*)
    def noSources                    = p in file("target/helper/" + p.id)
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

  private def loudLog(msg: String)(f: String => Unit): Unit = {
    f("")
    f("<*> " + msg)
    f("")
  }

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

  lazy val api         = project setup "psp's non-standard api"
  lazy val dmz         = project setup "psp's non-standard dmz"
  lazy val std         = project setup "psp's non-standard standard library" dependsOn (api, dmz) also spire
  lazy val publishOnly = project.helper.noSources aggregate (projectRefs: _*)
  lazy val compileOnly = project.helper.noSources aggregate (projectRefs: _*)
  lazy val testOnly    = project.helper aggregate (projectRefs: _*) settings (
          testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test  :=  false,
                  logBuffered  :=  false,
          libraryDependencies <++= testDependencies,
                         test  :=  (run in Test toTask "").value
  )

  def ammonite                     =  "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.4.8"
  def stdForkOptions               = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true)
  def consoleClasspathFiles        = fullClasspath in Compile in "consoleOnly" map (_.files)
  def consoleClasspathString       = consoleClasspathFiles map (_ mkString ":")
  def forkConfig                   = ForkConfig("psp.ReplMain", ImmutableProperties.empty, sciSeq("-usejavacp"), stdForkOptions)
  def forkRepl: TaskOf[ForkConfig] = Def task (forkConfig addJvmOptions ("-cp", consoleClasspathString.value))

  def asInputTask(task: TaskOf[ForkConfig]): InputTaskOf[Int] = Def inputTask task.value(spaceDelimited("<arg>").parsed: _*)
  def asTask(task: TaskOf[ForkConfig]): TaskOf[Int]           = asInputTask(task).toTask("")

  // A console project which pulls in misc additional dependencies currently being explored.
  // Removing all scalac options except the ones listed here, to eliminate all the warnings
  lazy val consoleOnly = (
    project.helper.usesCompiler.alsoToolsJar
    dependsOn (testOnly % "test->test")
    dependsOn (classpathDeps: _*)
    also (jsr305, ammonite)
    settings (console in Compile := asTask(forkRepl).value)
  )

  def testDependencies = Def setting Seq(
    Deps.scalaReflect.value,
    scalacheck.copy(configurations = None)
  )
}
