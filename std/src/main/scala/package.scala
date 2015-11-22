package psp

import scala.{ collection => sc }
import sc.{ immutable => sci }

package object std extends psp.std.StdPackageObject {
  type AdditiveMonoid[A]          = spire.algebra.AdditiveMonoid[A]
  type AdditiveSemigroup[A]       = spire.algebra.AdditiveSemigroup[A]
  type BooleanAlgebra[R]          = spire.algebra.Bool[R]
  type Monoid[A]                  = spire.algebra.Monoid[A]
  type MultiplicativeMonoid[A]    = spire.algebra.MultiplicativeMonoid[A]
  type MultiplicativeSemigroup[A] = spire.algebra.MultiplicativeSemigroup[A]
  type Natural                    = spire.math.Natural
  type UInt                       = spire.math.UInt

  type IntRange   = Consecutive[Int]
  type LongRange  = Consecutive[Long]
  type IndexRange = Consecutive[api.Index]

  final val ->        = psp.api.Pair
  final val Array     = scala.Array
  final val Failure   = scala.util.Failure
  final val NoIndex   = Index.invalid
  final val None      = scala.None
  final val Option    = scala.Option
  final val Some      = scala.Some
  final val Success   = scala.util.Success
  final val Try       = scala.util.Try
  final val sciList   = sci.List
  final val sciMap    = sci.Map
  final val sciSeq    = sci.Seq
  final val sciSet    = sci.Set
  final val sciVector = sci.Vector
  final val scmMap    = sc.mutable.Map

  final val NoFile: jFile = jFile("")
  final val NoPath: jPath = jPath("")
  final val NoUri: jUri   = jUri("")
}
