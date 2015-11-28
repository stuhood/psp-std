package psp
package api

/** API level type classes and interfaces other than the collections.
 */
import Api._

/** The classic type classes for encoding value equivalence and hash codes.
 */
trait Eq[-A]   extends Any            { def eqv(x: A, y: A): Boolean }
trait Hash[-A] extends Any with Eq[A] { def hash(x: A): Int          }

/** The classic type class for turning typed values into string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

/** The original type class for providing the "empty" value of a particular type.
 *  Suitable only for types with a unique (useful) definition of empty - but that's
 *  a whole lot of types.
 */
trait Empty[@fspec +A] extends Any {
  def empty: A
  def isEmptyValue[A1 >: A](x: A1): Boolean = x == empty
}

/** Back and forth between a Repr and an Each[A].
 *  Not especially classic in this presentation.
 */
trait Builds[@fspec -Elem, +To] extends Any {
  def build(xs: Foreach[Elem]): To
}

trait Unbuilds[Repr] extends Any {
  type Elem
  def unbuild(xs: Repr): Foreach[Elem]
}

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the `2^32` states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[@fspec -A] extends Any with Eq[A] {
  def cmp(x: A, y: A): Cmp
}

/** Type classes and extractors for composing and decomposing an R into A -> B.
 *  Somewhat conveniently for us, "cleave" is a word which has among its meanings
 *  "to adhere firmly and closely as though evenly and securely glued" as well
 *  as "to divide into two parts by a cutting blow".
 */

trait Splitter[-R, +A, +B] extends Any {
  def split(x: R): A -> B
}
trait Joiner[+R, -A, -B]  extends Any {
  def join(x: A -> B): R
  def join(x: A, y: B): R
}
trait Cleaver[R, A, B] extends Any with Joiner[R, A, B] with Splitter[R, A, B]

object Cleaver {
  def apply[R, A, B](f: (A, B) => R, l: R => A, r: R => B): Cleaver[R, A, B] = new Cleaver[R, A, B] {
    def split(x: R): A -> B = (l(x), r(x))
    def join(x: A, y: B): R = f(x, y)
    def join(x: A -> B): R  = f(x._1, x._2)
  }
}
object Splitter {
  def apply[R, A, B](f: R => (A -> B)): Splitter[R, A, B] =
    new Splitter[R, A, B] { def split(x: R): A -> B = f(x) }

  def apply[R, A, B](l: R => A, r: R => B): Splitter[R, A, B] =
    new Splitter[R, A, B] { def split(x: R): A -> B = (l(x), r(x)) }
}
object Joiner {
  def apply[R, A, B](f: (A, B) => R): Joiner[R, A, B] =
    new Joiner[R, A, B] {
      def join(x: A, y: B): R = f(x, y)
      def join(x: A -> B): R  = f(x._1, x._2)
    }
}

object Pair {
  def apply[R, A, B](x: A -> B)(implicit z: Joiner[R, A, B]): R           = z.join(x)
  def apply[R, A, B](x: A, y: B)(implicit z: Joiner[R, A, B]): R          = z.join(x, y)
  def unapply[R, A, B](x: R)(implicit z: Splitter[R, A, B]): Some[A -> B] = some(z split x)
}
