package psp
package impl

import api._, std._, StdEq._
import scala.{ collection => sc }

object Size {
  val Empty    = api.Size(0)
  val NonEmpty = api.Size(1, Infinite)
  val Unknown  = api.Size(Empty, Infinite)

  def empty: Size   = Empty
  def unknown: Size = Unknown
  def apply(n: Long): Precise = Size(n)

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

  def bounded(lo: Size, hi: Size): Size = (lo, hi) match {
    case _ if lo === hi                     => lo
    case (lo: Precise, hi: Atomic)          => api.Size(lo, hi)
    case (l1: Precise, Bounded(l2, h2))     => api.Size(min(l1, l2), h2)
    case (Infinite, Bounded(_, Infinite))   => Infinite
    case (Infinite, _)                      => Empty
    case (Bounded(l1, h1), Infinite)        => api.Size(l1, Infinite)
    case (Bounded(l1, h1), h2: Precise)     => api.Size(l1, h2)
    case (Bounded(l1, h1), Bounded(l2, h2)) => api.Size(l1, h2)
  }
}
