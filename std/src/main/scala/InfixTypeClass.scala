package psp
package std
package infix

import api._

final class OrderOps[A](val lhs: A) extends AnyVal {
  import Cmp._
  def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.compare(lhs, rhs)

  def < (rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) eq LT
  def <=(rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) ne GT
  def > (rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) eq GT
  def >=(rhs: A)(implicit ord: Order[A]): Boolean = compare(rhs) ne LT

  // Having implicit infix max and min interferes with having implicit
  // postfix max and min on collections.
  def min2(rhs: A)(implicit ord: Order[A]): A = if (this < rhs) lhs else rhs
  def max2(rhs: A)(implicit ord: Order[A]): A = if (this > rhs) lhs else rhs
}
final class AlgebraOps[A](val lhs: A) extends AnyVal {
  def ^(rhs: A)(implicit z: BooleanAlgebra[A]): A       = (lhs || rhs) && !(lhs && rhs) // xor
  def implies(rhs: A)(implicit z: BooleanAlgebra[A]): A = !lhs || rhs
  def && (rhs: A)(implicit z: BooleanAlgebra[A]): A     = if (isOne) rhs else if (rhs.isOne) lhs else if (lhs.isZero || rhs.isZero) z.zero else z.and(lhs, rhs)
  def || (rhs: A)(implicit z: BooleanAlgebra[A]): A     = if (isZero) rhs else if (rhs.isZero) lhs else if (lhs.isOne || rhs.isOne) z.one else z.or(lhs, rhs)
  def unary_!(implicit z: BooleanAlgebra[A]): A         = if (isZero) z.one else if (isOne) z.zero else z.complement(lhs)
  def isZero(implicit z: BooleanAlgebra[A]): Boolean    = z.zero id_== lhs
  def isOne(implicit z: BooleanAlgebra[A]): Boolean     = z.one id_== lhs
}
final class EqOps[A](val lhs: A) extends AnyVal {
  def isEqualBy[B: Eq](f: A => B)(rhs: A): Boolean = f(lhs) === f(rhs)
  def ===(rhs: A)(implicit eq: Eq[A]): Boolean     = eq.equiv(lhs, rhs)
  def !==(rhs: A)(implicit eq: Eq[A]): Boolean     = !eq.equiv(lhs, rhs)
}
final class HashOps[A](val lhs: A) extends AnyVal {
  def hash(implicit z: Hash[A]): Int = z hash lhs
}
