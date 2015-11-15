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
