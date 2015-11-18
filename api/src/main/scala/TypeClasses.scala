package psp
package api

/** API level type classes and interfaces other than the collections.
 */
import Api._

/** The classic type classes for encoding value equivalence and hash codes.
 */
trait Eq[-A]   extends Any            { def equiv(x: A, y: A): Boolean }
trait Hash[-A] extends Any with Eq[A] { def hash(x: A): Int            }

/** The classic type class for turning typed values into string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

/** The original type class for providing the "empty" value of a particular type.
 *  Suitable only for types with a unique (useful) definition of empty - but that's
 *  a whole lot of types.
 */
trait Empty[@spec(SpecTypes) +A] extends Any { def empty: A }

/** Back and forth between a Repr and an Each[A].
 *  Not especially classic in this presentation.
 */
trait Builds[@spec(SpecTypes) -Elem, +To] extends Any {
  def build(xs: Each[Elem]): To
}
trait Unbuilds[Repr] extends Any {
  type Elem
  def unbuild(xs: Repr): Each[Elem]
}

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the `2^32` states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A] extends Any with Eq[A] {
  def compare(x: A, y: A): Cmp
  def equiv(x: A, y: A): Boolean = compare(x, y) == Cmp.EQ
}

/** Type classes and extractors for composing and decomposing an R into A -> B.
 *  Somewhat conveniently for us, "cleave" is a word which has among its meanings
 *  "to adhere firmly and closely as though evenly and securely glued" as well
 *  as "to divide into two parts by a cutting blow".
 */
object Pair {
  trait Join[+R, -A, -B]  extends Any { def join(x: A, y: B): R }
  trait Split[-R, +A, +B] extends Any { def split(x: R): A -> B }
  trait Cleave[R, A, B]   extends Any with Join[R, A, B] with Split[R, A, B]

  def apply[R, A, B](x: A, y: B)(implicit z: Join[R, A, B]): R         = z.join(x, y)
  def unapply[R, A, B](x: R)(implicit z: Split[R, A, B]): Some[A -> B] = some(z split x)

  object Cleave {
    def apply[R, A, B](f: (A, B) => R, l: R => A, r: R => B): Cleave[R, A, B] = new Cleave[R, A, B] {
      def split(x: R): A -> B = ((l(x), r(x)))
      def join(x: A, y: B): R = f(x, y)
    }
  }
  object Split {
    def apply[R, A, B](l: R => A, r: R => B): Split[R, A, B] =
      new Split[R, A, B] { def split(x: R): A -> B = ((l(x), r(x))) }
  }
  object Join {
    def apply[R, A, B](f: (A, B) => R): Join[R, A, B] =
      new Join[R, A, B] { def join(x: A, y: B): R = f(x, y) }
  }
}
