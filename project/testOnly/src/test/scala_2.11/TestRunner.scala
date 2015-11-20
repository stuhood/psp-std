package psp
package tests

import std._, Prop.forAll

object TestRunner_211 extends TestRunnerCommon {
  def scalaVersion = "2.11"

  lazy val bundles = commonBundles ++ vec[Bundle](
    new Typecheck,
    new AlgebraSpec[Boolean]("Boolean") { override def join = "||" ; override def meet = "&&" },
    new AlgebraSpec[InvariantPredicate[Pint]]("InvariantPredicate[Pint]")
  )
}
