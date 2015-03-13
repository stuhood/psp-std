package psp
package std

import api._

object Hash {
  def reference[A](): Hash[Ref[A]]   = ReferenceHash
  def natural[A](): Hash[A]          = NaturalHash
  def apply[A](f: ToInt[A]): Hash[A] = new impl.HashImpl[A](f)
}
object Eq {
  def reference[A](): Eq[Ref[A]]           = ReferenceEq
  def natural[A](): Eq[A]                  = NaturalEq
  def shown[A](implicit z: Show[A]): Eq[A] = eqBy[A](z.show)(NaturalEq)
  def apply[A](f: Relation[A]): Eq[A]      = new impl.EqImpl[A](f)

  class EqComparator[A: Eq]() extends Comparator[A] {
    def compare(x: A, y: A): Int = if (x === y) 0 else x.id_## - y.id_##
  }
  def eqComparator[A: Eq](): Comparator[A] = new EqComparator[A]
}

class WrapEqHashShow[A](equiv: Relation[A], hash: ToInt[A], show: ToString[A]) {
  def apply(x: A): Wrap = new Wrap(x)

  final class Wrap(val value: A) {
    override def equals(that: Any): Boolean = that match {
      case x: Wrap => equiv(value, x.value)
      case _       => false
    }
    override def hashCode = hash(value)
    override def toString = show(value)
  }
}

object WrapEqHashShow {
  def apply[A](implicit z1: Eq[A] = null, z2: Hash[A] = null, z3: Show[A] = null): WrapEqHashShow[A] = new WrapEqHashShow[A](
    (x, y) => if (z1 == null) x == y else z1.equiv(x, y),
    x => if (z2 == null) x.## else z2 hash x,
    x => if (z3 == null) "" + x else z3 show x
  )
}
