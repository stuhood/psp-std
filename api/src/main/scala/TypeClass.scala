package psp
package api

/** API level type classes and interfaces other than the collections.
 */
import Api._

/** The classic type classes for encoding value equivalence and hash codes.
 */
trait Eq[-A]     extends Any                         { def equiv(x: A, y: A): Boolean }
trait Hash[-A]   extends Any                         { def hash(x: A): Int            }
trait HashEq[-A] extends Any with Hash[A] with Eq[A]

/** The classic type class for turning string representations into typed values.
 */
trait Read[A] extends Any { def read(x: String): A }

/** The classic type class for turning typed values into string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

/** The original type class for providing the "empty" value of a particular type.
 *  Suitable only for types with a unique (useful) definition of empty - but that's
 *  a whole lot of types.
 */
trait Empty[+A] extends Any { def empty: A }

/** The collections builder type class. Not especially classic in this presentation.
 */
trait Builds[-Elem, +To] extends Any { def build(xs: Each[Elem]): To }

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A] extends Any { def compare(x: A, y: A): Cmp }

/** Name-based extractor methods. These interfaces aren't necessary
 *  for it (thus "name-based") but provide helpful structure when used.
 */
trait IsEmpty extends Any              { def isEmpty: Boolean }
trait Opt[+A] extends Any with IsEmpty { def get: A           }
trait Index extends Any with Opt[Long]

/** Type classes and extractors for composing and decomposing an R into A -> B.
 */
object Pair {
  trait Join[+R, -A, -B]  { def join(x: A, y: B): R }
  trait Split[-R, +A, +B] { def left(x: R): A ; def right(x: R): B ; def pair(x: R): A -> B }

  def apply[R, A, B](x: A, y: B)(implicit z: Join[R, A, B]): R         = z.join(x, y)
  def unapply[R, A, B](x: R)(implicit z: Split[R, A, B]): Some[A -> B] = some(z pair x)

  object Split {
    def apply[R, A, B](l: R => A, r: R => B): Split[R, A, B] = new Split[R, A, B] {
      def left(x: R): A      = l(x)
      def right(x: R): B     = r(x)
      def pair(x: R): A -> B = ((left(x), right(x)))
    }
  }
  object Join {
    def apply[R, A, B](f: (A, B) => R): Join[R, A, B] =
      new Join[R, A, B] { def join(x: A, y: B): R = f(x, y) }
  }
}

/** Generalized type constraint.
 */
sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A] extends <:<[A, A] { def apply(x: A): A = x }
