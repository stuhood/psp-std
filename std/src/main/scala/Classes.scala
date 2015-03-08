package psp
package std

import api._

/** Motley objects for which a file of residence is not obvious.
 */
object Pair {
  def apply[R, A, B](x: A, y: B)(implicit z: PairUp[R, A, B]): R          = z.create(x, y)
  def unapply[R, A, B](x: R)(implicit z: PairDown[R, A, B]): Some[(A, B)] = Some((z left x, z right x))
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
    case f: PartialFunction[_, _] => f isDefinedAt x
    case _                        => true
  }
  def apply(x: T): R = f(x)
}
trait LowPriorityUnsafe {
  // We may as well derive some convenience from the absence of parametricity.
  implicit def universalOrder[A] : Order[A] = orderBy[A]("" + _) | (_.##)
}
