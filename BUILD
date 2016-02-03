#
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
  name         = 'test-support',
  sources      = zglobs('test/support/*'),
  dependencies = [ ':std', ':scalacheck' ]
)
scala_library(
  name         = 'test-logic',
  sources      = zglobs('test/tests/*.scala'),
  dependencies = [ ':test-support' ]
)
jvm_binary(
  name         = 'test',
  main         = 'psp.tests.TestRunner_211',
  dependencies = [ ':test-logic' ]
)
