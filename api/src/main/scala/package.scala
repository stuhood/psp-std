package psp

package object api {
  // Inlinable constants.
  final val MaxInt           = scala.Int.MaxValue
  final val MaxLong          = scala.Long.MaxValue
  final val MinInt           = scala.Int.MinValue
  final val MinLong          = scala.Long.MinValue
  final val PositiveInfinity = scala.Double.PositiveInfinity

  // Other stable constants.
  final val ConstantTrue  = (x: scala.Any) => true
  final val ConstantFalse = (x: scala.Any) => false
  final val CTag          = scala.reflect.ClassTag
  final val EOL           = java.lang.System.getProperty("line.separator")
}

package api {
  sealed abstract class <:<[-From, +To] extends (From => To)
  final class conformance[A] extends <:<[A, A] { def apply(x: A): A = x }
}
