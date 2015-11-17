package psp
package tests

import psp._, std._, api._, StdShow._

abstract class TestRunnerCommon {
  def scalaVersion: String
  def bundles: Direct[Bundle]

  def shouldRun(b: Bundle) = sys.props get "psp.bundles" match {
    case Some(s) => b.bundle contains s
    case _       => true
  }

  def commonBundles = vec[Bundle](
    new StringExtensions,
    new GridSpec,
    new ViewBasic,
    new ViewSplitZip,
    new SizeSpec,
    new InferenceSpec,
    new CollectionsSpec,
    new SliceSpec,
    new OperationCounts,
    // new TokenSpec,
    new ResourcesSpec,
    new FunSpec
  )

  def wrapRun(b: Bundle): Boolean = Try(b.run) fold (
    t => andFalse(println(show"Caught $t running $b"), t.printStackTrace),
    identity
  )

  def main(args: Array[String]): Unit = {
    (bundles filter shouldRun).byEquals mapOnto wrapRun filterValues (x => !x) match {
      case ExMap()        => println(pp"\nAll tests passed.")
      case ExMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
  }
}
