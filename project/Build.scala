package psp
package build

import scala.Predef.{ conforms => _ }
import sbt._, Keys._, psp.libsbt._, Deps._
import psp.std._
import com.typesafe.sbt.JavaVersionCheckPlugin.autoImport._
import scoverage.ScoverageKeys._

object Build extends sbt.Build {
  def ammoniteDep         = "com.lihaoyi" % "ammonite-repl_2.11.7" % "0.4.8"
  def consoleDependencies = List(jsr305, ammoniteDep)
  def optimizeArgs        = wordSeq("-optimise -Yinline-warnings")
  def stdArgs             = wordSeq("-Yno-predef -Yno-adapted-args -Yno-imports -unchecked") // -Ymacro-debug-verbose
  def testDependencies    = Def setting Seq(Deps.scalaReflect.value, scalacheck.copy(configurations = None))

  lazy val api = project setup "psp's non-standard api"
  lazy val std = project setup "psp's non-standard standard library" dependsOn api also spire

  /***
   *** Everything below this line is to navigate the maze that is sbt.
   ***/

  def subprojects   = List[sbt.Project](api, std)
  def projectRefs   = convertSeq(subprojects): List[ProjectReference]
  def classpathDeps = convertSeq(subprojects): List[ClasspathDep[ProjectReference]]

  object ammoniteSupport {
    def consoleClasspathFiles        = fullClasspath in Compile in "consoleOnly" map (_.files)
    def consoleClasspathString       = consoleClasspathFiles map (_ mkString ":")
    def forkConfig                   = ForkConfig("psp.ReplMain", ImmutableProperties.empty, sciSeq("-usejavacp"), stdForkOptions)
    def forkRepl: TaskOf[ForkConfig] = Def task (forkConfig addJvmOptions ("-cp", consoleClasspathString.value))
    def setting                      = console in Compile := asTask(forkRepl).value
    def stdForkOptions               = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true)

    def asInputTask(task: TaskOf[ForkConfig]): InputTaskOf[Int] = Def inputTask task.value(spaceDelimited("<arg>").parsed: _*)
    def asTask(task: TaskOf[ForkConfig]): TaskOf[Int]           = asInputTask(task).toTask("")
  }

  implicit class ProjectOps(val p: Project) {
    def aggregatesAll = p aggregate (projectRefs: _*)
    def allSources    = Def task (sources in Test in p).value ++ (sources in Compile in p).value
    def usesCompiler  = p settings (libraryDependencies += Deps.scalaCompiler.value)
    def usesReflect   = p settings (libraryDependencies += Deps.scalaReflect.value)
    def usesAmmonite  = p settings ammoniteSupport.setting

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
                        triggeredMessage :=  Watched.clearWhenTriggered,
                       publishMavenStyle :=  true,
   javaVersionPrefix in javaVersionCheck :=  Some("1.7")
  ) ++ (
    if (p.id == "root") Nil
    else Seq(target <<= javaCrossTarget(p.id))
  )

  lazy val root = project.root.setup dependsOn (classpathDeps: _*) settings (
                  test <<=  test in testOnly,
    console in Compile <<=  console in Compile in consoleOnly,
       console in Test <<=  console in Test in consoleOnly,
          watchSources <++= testOnly.allSources,
          watchSources <++= consoleOnly.allSources
  )

  lazy val consoleOnly = project.helper.usesCompiler.usesAmmonite dependsOn (testOnly % "test->test") deps (consoleDependencies: _*)
  lazy val testOnly    = project.helper aggregate (projectRefs: _*) settings (
          testOptions in Test  +=  Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "1"),
    parallelExecution in Test  :=  false,
                  logBuffered  :=  false,
          libraryDependencies <++= testDependencies,
                         test  :=  (run in Test toTask "").value
  )
}
