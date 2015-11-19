package psp
package api

/** Sealed ADTs embedded in the API bedrock.
 */
import Api._

/** The Size hierarchy is:
 *                   Size
 *                  /        \
 *               Atomic      Bounded
 *              /      \
 *          Infinite  Precise
 *
 *  Precise implies the exact size is known. Infinite means it's infinite.
 *  Bounded is a size lower bound and a (possibly infinite) atomic upper bound.
 *  Size forms a partial order, with some liberties taken at present.
 *  Operations on sizes which are ill-defined will result in "Unknown", which
 *  encodes no available size information: Bounded(Zero, Infinite).
 *
 *  Invariants:
 *  - Precise is non-negative
 */

sealed trait Size extends Any
sealed trait Atomic extends Any with Size
final case object Infinite                                      extends Atomic
final case class Bounded private[api] (lo: Precise, hi: Atomic) extends Size
final case class Precise private[api] (value: Long)             extends AnyVal with Atomic {
  def min(that: Precise): Precise = if (value <= that.value) this else that
  def max(that: Precise): Precise = if (value >= that.value) this else that

  def +(n: Long): Precise = Size(value + n)
  def -(n: Long): Precise = Size(value - n)
  def -- : Precise        = Size(value - 1)
  def ++ : Precise        = Size(value + 1)
  def intValue: Int       = value.toInt
  def longValue: Long     = value
  override def toString   = s"$value"
}


object Size {
  val Zero    = Precise(0)
  val Unknown = Bounded(Zero, Infinite)

  /** Preserving associativity/commutativity of Size prevents us from
   *  modifying values to enforce any invariants on Bounded.
   */
  def apply(size: Long): Precise              = Precise( if (size < 0L) 0L else size )
  def apply(lo: Precise, hi: Atomic): Bounded = Bounded(lo, hi)
}
object PreciseInt {
  private val Max = scala.Int.MaxValue.toLong
  def unapply(x: Precise): Option[Int] = if (x.value <= Max) some(x.value.toInt) else none
}


/** A richer function abstraction.
 *
 *  No way to avoid at least having apply as a member method if there's
 *  to be any hope of seeing these converted into scala.Functions.
 */
sealed abstract class Fun[-A, +B] {
  final def apply(x: A): B = this match {
    case Opaque(g)       => g(x)
    case OrElse(u1, u2)  => if (u1 isDefinedAt x) u1(x) else u2(x)
    case Defaulted(g, u) => if (u isDefinedAt x) u(x) else g(x)
    case FilterIn(_, u)  => u(x) // filter is checked at isDefinedAt
    case AndThen(u1, u2) => u2(u1(x))
    case FiniteDom(_, g) => g(x)
  }
  final def isDefinedAt(x: A): Boolean = this match {
    case Opaque(_)       => true
    case OrElse(u1, u2)  => (u1 isDefinedAt x) || (u2 isDefinedAt x)
    case FilterIn(p, u)  => p(x) && (u isDefinedAt x)
    case Defaulted(_, u) => u isDefinedAt x
    case AndThen(u1, u2) => (u1 isDefinedAt x) && (u2 isDefinedAt u1(x))
    case FiniteDom(p, _) => p(x)
  }
}
final case class Opaque[-A, +B](f: A => B)                       extends Fun[A, B]
final case class Defaulted[-A, +B](g: A => B, u: Fun[A, B])      extends Fun[A, B]
final case class FilterIn[-A, +B](p: A => Boolean, u: Fun[A, B]) extends Fun[A, B]
final case class OrElse[-A, +B](f: Fun[A, B], g: Fun[A, B])      extends Fun[A, B]
final case class AndThen[-A, B, +C](f: Fun[A, B], g: Fun[B, C])  extends Fun[A, C]
final case class FiniteDom[A, +B](keys: ExSet[A], f: Fun[A, B])  extends Fun[A, B]

/** A not very impressive attempt to improve on string
 *  representations.
 */
sealed abstract class Doc

object Doc {
  final case object NoDoc                             extends Doc
  final case class Group(xs: View[Doc])               extends Doc
  final case class Cat(left: Doc, right: Doc)         extends Doc
  final case class Shown[A](value: A, shows: Show[A]) extends Doc
  final case class Literal(value: String)             extends Doc

  def empty: Doc = NoDoc
  def apply[A](x: A)(implicit z: Show[A]): Shown[A] = Shown[A](x, z)
  def apply(s: String): Literal                     = Literal(s)
}
