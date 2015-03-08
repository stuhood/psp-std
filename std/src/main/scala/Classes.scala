package psp
package std

import api._

/** Motley objects for which a file of residence is not obvious.
 */
object Pair {
  def apply[R, A, B](x: A, y: B)(implicit z: PairUp[R, A, B]): R          = z.create(x, y)
  def unapply[R, A, B](x: R)(implicit z: PairDown[R, A, B]): Some[A -> B] = Some((z left x, z right x))
}
object +: {
  def unapply[A](xs: Array[A])       = if (xs.length == 0) None else Some(xs.head -> xs.tail)
  def unapply[A](xs: Each[A])        = xs match { case Each(hd, _*) => Some(hd -> xs.tail) ; case _ => None }
  def unapply[A](xs: sCollection[A]) = if (xs.isEmpty) None else Some(xs.head -> xs.tail)
}
object StdEq extends impl.EqInstances
object StdShow extends ShowInstances
object Unsafe extends LowPriorityUnsafe {
  implicit def universalEq[A] : HashEq[A]        = HashEq.natural()
  implicit def universalShow[A] : Show[A]        = Show.natural()
  implicit def showableOrder[A: Show] : Order[A] = orderBy[A](_.to_s)
}

/** Motley classes for which a file of residence is not obvious.
 */
sealed trait IsDefault[+A] { def value: A }
final class DefaultFunction[+A](valueFn: => A) extends IsDefault[A] { def value = valueFn }
final class DefaultValue[+A](val value: A) extends IsDefault[A]

final case class Classpath(value: String) extends AnyVal { override def toString = value }
final case class Version(value: String) extends AnyVal   { override def toString = value }

trait AndThis { def andThis(x: Unit, xs: Unit*): this.type = this }

final class Utf8(val bytes: Array[Byte]) extends AnyVal with ForceShowDirect {
  def chars: Chars = scala.io.Codec fromUTF8 bytes
  def to_s: String = new String(chars)
}
class FunctionEqualizer[A, B : Eq](f: A => B, g: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A) = f(x) === g(x)
  def apply(x: A): B    = f(x)
  def forall(xs: Each[A]): Boolean = xs forall isDefinedAt
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) with ForceShowDirect {
  def isDefinedAt(x: T) = f match {
    case f: ?=>[_,_] => f isDefinedAt x
    case _           => true
  }
  def apply(x: T): R = f(x)
}
trait LowPriorityUnsafe {
  // We may as well derive some convenience from the absence of parametricity.
  implicit def universalOrder[A] : Order[A] = orderBy[A]("" + _) | (_.##)
}

final case class Split[A](left: View[A], right: View[A]) extends api.SplitView[A] {
  def mapLeft(f: View[A] => View[A]): Split[A]  = Split(f(left), right)
  def mapRight(f: View[A] => View[A]): Split[A] = Split(left, f(right))
  def rejoin: View[A]                           = left ++ right
}

/** Zipped0 means we're using a PairDown to interpret a collection holding As.
 *  Zipped1 means there's only one underlying View[A1->A2].
 *  Zipped2 means there are two collections, a View[A1] and a View[A2].
 *  This is plus or minus only a performance-related implementation detail.
 */
final case class Zipped0[A, A1, A2](xs: View[A])(implicit z: PairDown[A, A1, A2]) extends ZipView[A1, A2] {
  def lefts  = xs map z.left
  def rights = xs map z.right
  def pairs  = xs map z.pair
}
final case class Zipped1[A1, A2](pairs: View[A1 -> A2]) extends ZipView[A1, A2] {
  def lefts  = pairs map fst
  def rights = pairs map snd
}
final case class Zipped2[A1, A2](lefts: View[A1], rights: View[A2]) extends ZipView[A1, A2] {
  def pairs: View[A1 -> A2] = inView(mf => this.foreach((x, y) => mf(x -> y)))
}
