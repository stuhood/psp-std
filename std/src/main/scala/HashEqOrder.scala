package psp
package std

import api._

object Eq {
  val Inherited = hash[Any](_ == _)(_.##)
  val Reference = hash[AnyRef](_ eq _)(_.id_##)
  val ToString  = inherit[String] on ((x: Any) => "" + x)

  def apply[A](e: Relation[A]): Eq[A]               = new EqImpl[A](e)
  def hash[A](e: Relation[A])(h: ToInt[A]): Hash[A] = new HashImpl[A](e, h)
  def reference[A](): Hash[Ref[A]]                  = Reference
  def inherit[A](): Hash[A]                         = Inherited

  final class HashImpl[-A](e: Relation[A], h: ToInt[A]) extends api.Hash[A] {
    def equiv(x: A, y: A) = e(x, y)
    def hash(x: A): Int   = h(x)
  }
  final class EqImpl[-A](val e: Relation[A]) extends AnyVal with Eq[A] {
    def equiv(x: A, y: A) = e(x, y)
  }
  class EqComparator[A: Eq]() extends Comparator[A] {
    def compare(x: A, y: A): Int = if (x === y) 0 else x.id_## - y.id_##
  }
  def eqComparator[A: Eq](): Comparator[A] = new EqComparator[A]
}

object Order {
  import Cmp._

  def order[A: Order] : Order[A] = ?
  def fold(xs: Cmp*): Cmp        = xs.m findOr (_ != EQ, EQ)

  def create[A](z: Comparator[A]): Order[A]              = new FromComparator[A](z)
  def comparator[A](implicit z: Order[A]): Comparator[A] = new ToOrdering(z)
  def ordering[A](implicit z: Order[A]): Ordering[A]     = new ToOrdering(z)

  def inherited[A <: Comparable[A]](): Impl[A] = fromInt[A](_ compareTo _)
  def apply[A](f: (A, A) => Cmp): Impl[A]      = new FromRelation[A](f)
  def fromInt[A](f: (A, A) => Int): Impl[A]    = apply[A]((x, y) => longCmp(f(x, y)))
  def fromLong[A](f: (A, A) => Long): Impl[A]  = apply[A]((x, y) => longCmp(f(x, y)))

  sealed abstract class Impl[-A](f: OrderRelation[A]) extends Order[A] { def compare(x: A, y: A): Cmp = f(x, y) }
  final class FromComparator[A](c: Comparator[A])     extends Impl[A]((x, y) => longCmp(c.compare(x, y)))
  final class FromRelation[A](f: OrderRelation[A])    extends Impl[A](f)
  final class ToOrdering[A](z: Order[A])              extends Ordering[A] { def compare(x: A, y: A): Int = z.compare(x, y).intValue }
}
