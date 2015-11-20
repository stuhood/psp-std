package psp
package std

import api._, StdShow._

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
class FormatFun(val fmt: String) extends (Any => String) with ForceShowDirect {
  def apply(x: Any): String = fmt format x
  def to_s = fmt
}
object ?=> {
  def apply[A, B](p: ToBool[A], f: A => B): A ?=> B = { case x if p(x) => f(x) }
  def unapply[A, B](f: Fun[A, B]) = Some((f isDefinedAt _, f apply _))
}
object sys {
  def error(msg: java.lang.String): Nothing = scala.sys.error(msg)
  def props                                 = scala.sys.props
  def env                                   = scala.sys.env
}
final case class FunctionGrid[A, B](values: View[A], functions: View[A => B]) {
  def rows: View2D[B]   = values map (v => functions map (f => f(v)))
  def columns:View2D[B] = functions map (f => values map (v => f(v)))

  def renderLines(implicit z: Show[B]): Vec[String]               = {
    val widths    = columns map (_ map z.show map (_.length) max)
    val formatFns = widths map leftFormatString

    rows map (formatFns zip _ map (_ apply _) mk_s ' ')
  }
  def render(implicit z: Show[B]): String = renderLines.joinLines
}

class Partial[A, B](p: ToBool[A], f: A => B) extends (A ?=> B) {
  def isDefinedAt(x: A): Boolean            = p(x)
  def apply(x: A): B                        = f(x)
  def applyOr(x: A, alt: => B): B           = if (p(x)) f(x) else alt
  def zapply(x: A)(implicit z: Empty[B]): B = applyOr(x, emptyValue)
}
object Partial {
  implicit def liftPartial[A, B](pf: A ?=> B): Partial[A, B] = apply(pf)
  def apply[A, B](pf: A ?=> B): Partial[A, B]                = apply(pf isDefinedAt _, pf apply _)
  def apply[A, B](p: ToBool[A], f: A => B): Partial[A, B]    = new Partial(p, f)
}

object Grid {
  def apply[A](f: A => (A, A)): A => View[A] = {
    def loop(x: A): View[A] = f(x) |> { case (a, b) => a +: loop(b) }
    loop
  }
}

object Empty {
  def empty[A] : Empty[A] = new Throws[A]("empty")
  def apply[A](empty: A): Impl[A] = new Impl[A](empty)

  final class Throws[A](msg: String) extends Empty[A] { def empty: A = abort(msg) }
  final class Impl[A](val empty: A) extends AnyVal with Empty[A]
}

object StdEq extends EqInstances
object StdShow extends ShowInstances
object Unsafe {
  implicit def inheritedEq[A] : Hash[A]       = inheritEq
  implicit def inheritedShow[A] : Show[A]     = inheritShow
  implicit def shownOrder[A: Show] : Order[A] = orderBy[A](render[A])
}

trait AndThis { def andThis(x: Unit, xs: Unit*): this.type = this }

final class Utf8(val bytes: Array[Byte]) extends AnyVal with ForceShowDirect {
  def chars: Array[Char] = scala.io.Codec fromUTF8 bytes
  def to_s: String       = new String(chars)
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
  def failed(msg: => String): Unit =
    new AssertionError(msg) |> (t => sideEffect(t.printStackTrace, throw t))
}

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = Order[A]((x, y) => z.cmp(f(x), f(y)))                     }
final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]   = Show[A](f andThen z.show)                                 }
final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]   = Eq.hash[A]((x, y) => z.eqv(f(x), f(y)))(x => z hash f(x)) }
final class EqBy[A]    { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]       = Eq[A]((x, y) => z.eqv(f(x), f(y)))                        }
