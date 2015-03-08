package psp
package std

import api._, StdEq._

class FunctionEqualizer[A, B : Eq](f: A => B, g: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A) = f(x) === g(x)
  def apply(x: A): B    = f(x)
  def forall(xs: Each[A]): Boolean = xs forall isDefinedAt
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) with ForceShowDirect {
  def isDefinedAt(x: T) = f match {
    case f: PartialFunction[_, _] => f isDefinedAt x
    case _                        => true
  }
  def apply(x: T): R = f(x)
}
