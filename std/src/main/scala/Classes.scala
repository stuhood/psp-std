package psp
package std

import api._

/** Motley objects for which a file of residence is not obvious.
 */
object HasSize {
  def unapply(x: HasSize): Some[Size] = some(x.size)
}
object +: {
  def unapply[A](xs: Array[A])       = if (xs.length == 0) None else Some(xs.head -> xs.tail)
  def unapply[A](xs: Each[A])        = xs match { case Each(hd, _*) => Some(hd -> xs.tail) ; case _ => None }
  def unapply[A](xs: sCollection[A]) = if (xs.isEmpty) None else Some(xs.head -> xs.tail)
}

object Empty {
  def apply[A](empty: A): Impl[A] = new Impl[A](empty)
  final class Impl[A](val empty: A) extends AnyVal with Empty[A]
}

object StdEq extends EqInstances
object StdShow extends ShowInstances
object Unsafe extends LowPriorityUnsafe {
  implicit def inheritedEq[A] : Hash[A]       = inheritEq
  implicit def inheritedShow[A] : Show[A]     = inheritShow
  implicit def shownOrder[A: Show] : Order[A] = orderBy[A](_.render)
}
trait LowPriorityUnsafe {
  // We may as well derive some convenience from the absence of parametricity.
  implicit def universalAdHocOrdering[A] : Order[A] = StringOrder | (_.##)
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
  def chars: Array[Char] = scala.io.Codec fromUTF8 bytes
  def to_s: String       = new String(chars)
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

trait Assertions {
  def failed(msg: => String): Unit
  def assert(assertion: Boolean, msg: => String): Unit = if (!assertion) failed(msg)
}
object Assertions {
  private[this] var instance: Assertions = DefaultAssertions
  def using[A](x: Assertions)(assertion: => Boolean, msg: => String): Unit = {
    val saved = instance
    instance = x
    try instance.assert(assertion, msg) finally instance = saved
  }
  implicit object DefaultAssertions extends Assertions {
    def failed(msg: => String): Unit = assertionError(msg)
  }
}
object ImmediateTraceAssertions extends Assertions {
  def failed(msg: => String): Unit = {
    val t = new AssertionError(msg)
    t.printStackTrace
    throw t
  }
}

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = z on f }
final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]   = z on f }
final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]   = z on f }
final class EqBy[A]    { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]       = z on f }
