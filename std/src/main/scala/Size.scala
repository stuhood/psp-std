package psp
package impl

import api._, std._, StdEq._

object Size {
  val Empty    = 0.size
  val NonEmpty = api.Size(1.size, Infinite)
  val Unknown  = api.Size(Empty, Infinite)

  def empty: Size   = Empty
  def unknown: Size = Unknown

  def min(s1: Precise, s2: Precise): Precise = if (s1 <= s2) s1 else s2
  def max(s1: Precise, s2: Precise): Precise = if (s1 >= s2) s1 else s2

  def hash(s: Size): Int = s match {
    case Precise(n) => n.##
    case x          => x.##
  }
  def equiv(s1: Size, s2: Size): Boolean = (s1, s2) match {
    case (Infinite, Infinite)               => true
    case (Precise(s1), Precise(s2))         => s1 === s2
    case (Bounded(l1, h1), Bounded(l2, h2)) => (l1 === l2) && (h1 === h2)
    case _                                  => false
  }

  def apply(n: Long): LongSize = Precise(n)
  def apply(n: Int): IntSize   = Precise(n)
  def apply(x: Any): Size = x match {
    case xs: HasSize                                => xs.size
    case xs: jCollection[_]                         => xs.size.size
    case xs: scIndexedSeq[_]                        => xs.size.size
    case xs: scTraversable[_] if xs.hasDefiniteSize => if (xs.isEmpty) Empty else NonEmpty
    case _                                          => Unknown
  }

  object GenBounded {
    def unapply(x: Size): Option[(Precise, Atomic)] = x match {
      case Bounded(lo, hi) => Some(lo -> hi)
      case x: Precise      => Some(x -> x)
      case _               => None
    }
  }
  // Return (lo, hi) as sizes unless arg is or contains Infinite.
  object Finite {
    def unapply(x: Size): Option[(Precise, Precise)] = x match {
      case Bounded(lo, hi: Precise) => Some(lo -> hi)
      case x: Precise               => Some(x -> x)
      case _                            => None
    }
  }

  def areComparable(lhs: Size, rhs: Size): Boolean = (lhs, rhs) match {
    case (Finite(_), Finite(_))                                     => true
    case (GenBounded(_, hi: Precise), GenBounded(lo, _)) if hi < lo => true
    case (GenBounded(lo, _), GenBounded(_, hi: Precise)) if hi < lo => true
    case _                                                          => false
  }

  def bounded(lo: Size, hi: Size): Size = (lo, hi) match {
    case _ if lo === hi                     => lo
    case (lo: Precise, hi: Atomic)          => api.Size(lo, hi)
    case (l1: Precise, Bounded(l2, h2))     => api.Size(l1 min l2, h2)
    case (Infinite, Bounded(_, Infinite))   => Infinite
    case (Infinite, _)                      => Empty
    case (Bounded(l1, h1), Infinite)        => api.Size(l1, Infinite)
    case (Bounded(l1, h1), h2: Precise)     => api.Size(l1, h2)
    case (Bounded(l1, h1), Bounded(l2, h2)) => api.Size(l1, h2)
  }
}
