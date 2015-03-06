package psp
package std

trait SpireIntegration {
  type BooleanAlgebra[R] = spire.algebra.Bool[R]
  type UInt              = spire.math.UInt
  type Natural           = spire.math.Natural
}
