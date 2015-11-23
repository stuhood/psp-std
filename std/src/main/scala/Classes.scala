package psp
package std

import api._, StdShow._

/** Motley objects for which a file of residence is not obvious.
 */
class FormatFun(val fmt: String) extends (Any => String) with ForceShowDirect {
  def apply(x: Any): String = fmt format x
  def to_s = fmt
}
object sys {
  def props = scala.sys.props
  def env   = scala.sys.env
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

object Empty {
  def empty[A] : Empty[A]            = new Throws[A]("empty")
  def apply[A](empty: => A): Impl[A] = new Impl[A](empty)
  def const[A](empty: A): Empty[A]   = new Const[A](empty)

  final class Throws[A](msg: String) extends Empty[A] { def empty: A = abort(msg) }
  final class Impl[A](expr: => A) extends Empty[A] { def empty: A = expr }
  final class Const[A](val empty: A) extends AnyVal with Empty[A]
}

object StdEq extends EqInstances
object StdShow extends ShowInstances {
  implicit def convertHasShowDocOps[A: Show](x: A): ops.DocOps      = new ops.DocOps(Doc(x))
  implicit def convertHasShowDoc[A](x: A)(implicit z: Show[A]): Doc = Doc(x)
}
object Unsafe {
  implicit def inheritedEq[A] : Hash[A]       = inheritEq
  implicit def inheritedShow[A] : Show[A]     = inheritShow
  implicit def shownOrder[A: Show] : Order[A] = orderBy[A](render[A])
}

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = Order[A]((x, y) => z.cmp(f(x), f(y)))                     }
final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]   = Show[A](f andThen z.show)                                 }
final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]   = Eq.hash[A]((x, y) => z.eqv(f(x), f(y)))(x => z hash f(x)) }
final class EqBy[A]    { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]       = Eq[A]((x, y) => z.eqv(f(x), f(y)))                        }
