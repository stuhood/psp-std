package psp
package ext

trait Spire {
  type AdditiveMonoid[A]          = spire.algebra.AdditiveMonoid[A]
  type AdditiveSemigroup[A]       = spire.algebra.AdditiveSemigroup[A]
  type BooleanAlgebra[R]          = spire.algebra.Bool[R]
  type Monoid[A]                  = spire.algebra.Monoid[A]
  type MultiplicativeMonoid[A]    = spire.algebra.MultiplicativeMonoid[A]
  type MultiplicativeSemigroup[A] = spire.algebra.MultiplicativeSemigroup[A]
  type Natural                    = spire.math.Natural
  type SafeLong                   = spire.math.SafeLong
  type UInt                       = spire.math.UInt
}
