package psp
package std
package ops

import api._

/** "Extensions" are classes which only exist to add methods to
 *  built-in types from the scala standard library. As we phase
 *  out the use of the standard library these will migrate into
 *  "Ops" classes, where we control the underlying class.
 */

final class PartialFunctionOps[A, B](val pf: A ?=> B) extends AnyVal {
  def zapply(x: A)(implicit z: Empty[B]): B = if (pf isDefinedAt x) pf(x) else z.empty
}

final class OptionOps[A](val x: Option[A]) extends AnyVal {
  def | (alt: => A): A                             = x getOrElse alt
  def ||(alt: => A): Option[A]                     = x orElse Some(alt)
  def |?[A1 >: A](alt: => A1): A1                  = x getOrElse alt
  def ||?[A1 >: A](alt: => Option[A1]): Option[A1] = x orElse alt

  def zfold[B: Empty](f: A => B): B    = x.fold[B](emptyValue)(f)
  def orEmpty(implicit z: Empty[A]): A = x getOrElse emptyValue[A]
  def or(alt: => A): A                 = x getOrElse alt
  def orFail(msg: String): A           = x getOrElse abort(msg)
  def toVec: Vec[A]                    = x.fold[Vec[A]](vec())(vec(_))
}

final class TryOps[A](val x: Try[A]) extends AnyVal {
  def | (expr: => A): A = x match {
    case Failure(_) => expr
    case Success(x) => x
  }
  def || (expr: => A): Try[A] = x match {
    case x @ Success(_) => x
    case Failure(_)     => Try(expr)
  }
  def fold[B](f: Throwable => B, g: A => B): B = x match {
    case Success(x) => g(x)
    case Failure(t) => f(t)
  }
}

/*** Java ***/

final class JavaIteratorOps[A](it: jIterator[A]) {
  def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
}

final class FileTimeOps(val time: FileTime) extends AnyVal {
  def isNewer(that: FileTime) = (time compareTo that) > 0
  def isOlder(that: FileTime) = (time compareTo that) < 0
  def isSame(that: FileTime)  = (time compareTo that) == 0
}

object infix {
  final class OrderOps[A](val lhs: A) extends AnyVal {
    import Cmp._
    def compare(rhs: A)(implicit ord: Order[A]): Cmp = ord.cmp(lhs, rhs)

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
}
