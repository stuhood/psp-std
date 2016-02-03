# TODO
#  - artifacts and publishing
#  - recover code coverage
scala_library(
  name         = 'api',
  sources      = zglobs('api/*'),
  dependencies = [ ':spire' ]
)
scala_library(
  name         = 'std',
  sources      = zglobs('std/*'),
  dependencies = [ ':api' ]
)
scala_library(
  name         = 'repl-support',
  sources      = zglobs('repl/*.scala'),
  dependencies = [ ':std', ':ammonite' ]
)
# Has to be separate from tests so macros are built first.
scala_library(
  name         = 'test-support',
  sources      = zglobs('test/support/*.scala'),
  dependencies = [ ':std', ':scalacheck', ':junit' ]
)
junit_tests(
  name         = 'test',
  sources      = zglobs('test/test/*.scala'),
  dependencies = [ ':test-support' ]
)

# TODO - how to make a console task which runs psp.ReplMain with the
# right options everywhere? We need a forked jvm invoked with the right
# classpath, which passes -usejavacp and other options to ReplMain.
jvm_binary(
  name         = 'repl',
  main         = 'psp.ReplMain',
  dependencies = [ ':repl-support' ]
)
# For reference, the task from sbt.
#
# def pspArgs = wordSeq("-encoding utf8 -language:_ -Yno-predef -Yno-imports -Yno-adapted-args")
# val ammoniteTask = Def task {
#   val forker    = new Fork("java", Some("psp.ReplMain"))
#   val files     = (fullClasspath in Compile in LocalProject("consoleOnly")).value.files filterNot (_.toString contains "scoverage")
#   val classpath = files mkString ":"
#   val jvmArgs   = sciSeq(s"-Xbootclasspath/a:$classpath") // boot classpath way faster
#   val forkOpts  = ForkOptions(outputStrategy = Some(StdoutOutput), connectInput = true, runJVMOptions = jvmArgs)
#   forker(forkOpts, "-usejavacp" +: pspArgs)
# }
