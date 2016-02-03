package psp
package tests

import psp._, std._, all._, Prop.forAll, StdShow._
import org.junit._

object TestRunner {
  def shouldRun(b: Bundle) = sys.props get "psp.bundles" match {
    case Some(s) => b.bundle contains s
    case _       => true
  }
  def bundles = vec[Bundle](
    new StringExtensions,
    new GridSpec,
    new ViewBasic,
    new ViewSplitZip,
    new InferenceSpec,
    new CollectionsSpec,
    new SliceSpec,
    new OperationCounts,
    new ADTSpec,
    new SpireSpec,
    new EmptySpec,
    new Typecheck,
    new AlgebraSpec[Boolean]("Boolean") { override def join = "||" ; override def meet = "&&" },
    new AlgebraSpec[InvariantPredicate[Pint]]("InvariantPredicate[Pint]")
  )

  def wrapRun(b: Bundle): Boolean = Try(b.run) fold (
    t => sideEffect(false, println(show"Caught $t running $b"), t.printStackTrace),
    identity
  )

  def main(args: Array[String]): Unit = {
    (bundles filter shouldRun).byEquals mapOnto wrapRun filterValues (x => !x) match {
      case ExMap()        => println(pp"\nAll tests passed.")
      case ExMap(ks @ _*) => println("Some tests failed in bundles: " + ks.mkString(", ")) ; throw new Exception
    }
    Benchmark2 main array()
  }
}


class JUnitTestRunner {
  @Test
  def allTests(): Unit = TestRunner.main(array())
}
