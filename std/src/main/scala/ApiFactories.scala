package psp
package std

import api._

object Order {
  import Cmp._

  def apply[A](f: (A, A) => Cmp): impl.OrderImpl[A]     = new impl.OrderImpl[A](f)
  def natural[A <: Comparable[A]](): impl.OrderImpl[A]  = fromInt[A](_ compareTo _)
  def order[A: Order] : Order[A]                        = ?
  def comparator[A: Order] : Comparator[A]              = implicitly[Order[A]].toComparator
  def fold(xs: Cmp*): Cmp                               = xs.m findOr (_ != EQ, EQ)
  def create[A](ord: Ordering[A]): Order[A]             = apply[A]((x, y) => longCmp(ord.compare(x, y)))
  def fromInt[A](f: (A, A) => Int): impl.OrderImpl[A]   = apply[A]((x, y) => longCmp(f(x, y)))
  def fromLong[A](f: (A, A) => Long): impl.OrderImpl[A] = apply[A]((x, y) => longCmp(f(x, y)))
}

object Empty {
  def apply[A](empty: A): Impl[A] = new Impl[A](empty)
  final class Impl[A](val empty: A) extends AnyVal with Empty[A]
}
