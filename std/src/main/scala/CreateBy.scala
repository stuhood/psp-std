package psp
package impl

import api._
import std._

final class OrderBy[A]   { def apply[B](f: A => B)(implicit z: Order[B]): Order[A]   = new OrderImpl[A]((x, y) => z.compare(f(x), f(y))) }
final class EqBy[A]      { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]         = new EqImpl[A]((x, y) => z.equiv(f(x), f(y)))      }
final class ShowBy[A]    { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]     = new ShowImpl[A](x => z show f(x))                 }
final class HashBy[A]    { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]     = new HashImpl[A](x => z hash f(x))                 }
final class HashEqBy[A]  { def apply[B](f: A => B)(implicit z: HashEq[B]): HashEq[A] = new HashEqImpl[A]((x, y) => z.equiv(f(x), f(y)), x => z hash f(x)) }

trait CreateBy {
  def eqBy[A]     = new EqBy[A]
  def hashBy[A]   = new HashBy[A]
  def hashEqBy[A] = new HashEqBy[A]
  def orderBy[A]  = new OrderBy[A]
  def showBy[A]   = new ShowBy[A]
}
object CreateBy extends CreateBy

final class OrderingImpl[A](cmp: (A, A) => Cmp)                    extends Ordering[A]         { def compare(x: A, y: A): Int = cmp(x, y).intValue }
final class ComparatorImpl[A](f: (A, A) => Int)                    extends Comparator[A]       { def compare(x: A, y: A): Int = f(x, y)            }
final class ShowImpl[-A](val f: A => String)                       extends AnyVal with Show[A] { def show(x: A) = f(x)                             }
final class HashImpl[-A](val f: A => Int)                          extends AnyVal with Hash[A] { def hash(x: A): Int = f(x)                        }
final class EqImpl[-A](val f: Relation[A])                         extends AnyVal with Eq[A]   { def equiv(x: A, y: A) = f(x, y)                   }
final class HashEqImpl[-A](isEquiv: Relation[A], hashFn: A => Int) extends HashEq[A]           {
  def equiv(x: A, y: A) = isEquiv(x, y)
  def hash(x: A)        = hashFn(x)
}
final class OrderImpl[-A](val f: (A, A) => Cmp) extends AnyVal with Order[A] {
  def compare(x: A, y: A): Cmp                = f(x, y)
  def toScalaOrdering[A1 <: A] : Ordering[A1] = new OrderingImpl[A1](compare)
  def toComparator[A1 <: A] : Comparator[A1]  = new ComparatorImpl[A1]((x, y) => f(x, y).intValue)
}
