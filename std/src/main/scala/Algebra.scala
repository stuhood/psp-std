package psp
package std

import api._

/** TODO - how to abstract the entire notion of a Complement class and the
 *  always-the-same logic which accompanies it, without virtual classes?
 */
object Algebras {
  final case class Not[A](f: ToBool[A]) extends ToBool[A] with ForceShowDirect {
    def apply(x: A): Boolean = !f(x)
    def to_s = "!" + f
  }
  final class PredicateAlgebra[A] extends BooleanAlgebra[ToBool[A]] {
    private type R = ToBool[A]

    /** TODO - one of of the benefits of having constant true and false is an
     *  opportunity to optimize expressions away entirely with no evaluation,
     *  if e.g. y is ConstantTrue in x(p) || y(p). Obviously this won't mix well
     *  with side effects. How enthusiastic can we be about punishing side effects
     *  before we kill the patient?
     */
    def and(x: R, y: R): R = p => x(p) && y(p)
    def or(x: R, y: R): R  = p => x(p) || y(p)
    def zero: R            = ConstantFalse
    def one: R             = ConstantTrue
    def complement(f: R): R = f match {
      case ConstantFalse => ConstantTrue
      case ConstantTrue  => ConstantFalse
      case Not(f)        => f
      case _             => Not(f)
    }
  }

  final class InSetAlgebra[A] extends BooleanAlgebra[InSet[A]] {
    import InSet._

    def and(x: InSet[A], y: InSet[A]): InSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => complement(Union[A](xs, ys))
      case (Complement(xs), ys)             => Diff[A](ys, xs)
      case (xs, Complement(ys))             => Diff[A](xs, ys)
      case _                                => Intersect[A](x, y)
    }
    def or(x: InSet[A], y: InSet[A]): InSet[A] = (x, y) match {
      case (Complement(xs), Complement(ys)) => complement(Intersect(xs, ys))
      case (Complement(xs), ys)             => complement(Diff(xs, ys))
      case (xs, Complement(ys))             => complement(Diff(ys, xs))
      case _                                => Union(x, y)
    }
    def complement(x: InSet[A]): InSet[A] = x match {
      case Zero           => one
      case One            => zero
      case Complement(xs) => xs            // unwrap
      case _              => Complement(x) // wrap
    }
    def zero: InSet[A] = Zero.castTo
    def one: InSet[A]  = One.castTo
  }
}
