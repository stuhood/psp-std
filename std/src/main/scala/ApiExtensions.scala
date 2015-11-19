package psp
package std
package ops

import api._, StdEq._, StdShow._
import java.io.BufferedInputStream

final class DocSeqOps(xs: Direct[Doc]) {
  def joinLines: String = xs mapNow (_.render) mk_s EOL
}
final class ExMapOps[K, V](xs: ExMap[K, V]) {
  type Entry = K -> V

  def keys: View[K]        = keySet.m
  def values: View[V]      = keyVector map xs.lookup
  def keySet: ExSet[K]     = xs.lookup.keys
  def keyVector: Vec[K]    = keys.toVec
  def entries: View[Entry] = keyVector mapZip xs.lookup
  def filterValues(p: ToBool[V]): ExMap[K, V] = xs filterKeys (k => p(xs(k)))
}
final class ExSetOps[A](xs: ExSet[A]) {
  def add(x: A): ExSet[A]                = xs + x
  def filter(p: ToBool[A]): ExSet[A]     = ExSet.Filtered(xs, p)
  def union(that: ExSet[A]): ExSet[A]    = ExSet.Union(xs, that)
  def mapOnto[B](f: A => B): ExMap[A, B] = ExMap(xs, Fun(f))
}

trait HasPreciseSizeMethods extends Any {
  def size: Precise

  def longSize: Long      = size.longValue
  def intSize: Int        = size.intValue
  def isPositive: Boolean = longSize > 0L
  def indices: IndexRange = indexRange(0, intSize)
  def lastIndex: Index    = Index(longSize - 1)  // effectively maps both undefined and zero to no index.

  def containsIndex(index: Index): Boolean          = indices.m contains index
  @inline def foreachIndex(f: Index => Unit): Unit  = if (isPositive) lowlevel.ll.foreachConsecutive(0, lastIndex.getInt, i => f(Index(i)))
  @inline def foreachIntIndex(f: Int => Unit): Unit = if (isPositive) lowlevel.ll.foreachConsecutive(0, lastIndex.getInt, f)
}

final class HasPreciseSizeOps(val x: HasPreciseSize) extends HasPreciseSizeMethods {
  def size: Precise = x.size
}

final class TimesBuilder(val times: Precise) {
  def const[A](elem: A): Each[A]   = Each const elem take times
  def eval[A](body: => A): Each[A] = Each continually body take times
}

final class PreciseOps(val size: Precise) extends AnyRef with HasPreciseSizeMethods {
  def getInt: Int      = size.intValue
  def times            = new TimesBuilder(size)
  def leftFormatString = if (getInt == 0) "%s" else "%%-%ds" format getInt

  def + (n: Precise): Precise = size + n.longValue
  def - (n: Precise): Precise = size - n.longValue
  override def toString = s"$longSize"
}

final class InputStreamOps(val in: InputStream) extends AnyVal {
  def buffered: BufferedInputStream = in match {
    case in: BufferedInputStream => in
    case _                       => new BufferedInputStream(in)
  }
  def slurp(): Array[Byte]             = lowlevel.Streams slurp buffered
  def slurp(len: Precise): Array[Byte] = lowlevel.Streams.slurp(buffered, len)
}

final class StdOptOps[A](val x: Opt[A]) extends AnyVal {
  def fold[B](none: => B)(f: A => B): B = if (x.isEmpty) none else f(x.get)
  def |[A1 >: A](alt: => A1): A1        = if (x.isEmpty) alt else x.get
}

final class SizeOps(val lhs: Size) extends AnyVal {
  import impl.Size._
  import StdEq._

  def isNonZero     = loBound !== Size.Zero
  def isZero        = lhs === Size.Zero
  def atLeast: Size = bounded(lhs, Infinite)
  def atMost: Size  = bounded(Empty, lhs)

  def loBound: Atomic = lhs match {
    case Bounded(lo, _) => lo
    case x: Atomic      => x
  }

  def slice(range: IndexRange): Size = (this - range.toDrop) min range.toTake

  /** For instance taking the union of two sets. The new size is
   *  at least the size of the larger operand, but at most the sum
   *  of the two sizes.
   */
  def union(rhs: Size): Size     = bounded(lhs max rhs, lhs + rhs)
  def intersect(rhs: Size): Size = bounded(0.size, lhs min rhs)
  def diff(rhs: Size): Size      = bounded(lhs - rhs, lhs)

  def + (rhs: Size): Size = (lhs, rhs) match {
    case (Infinite, _) | (_, Infinite)            => Infinite
    case (Precise(l), Precise(r))                 => l + r size
    case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 + l2, h1 + h2)
  }
  def - (rhs: Size): Size = (lhs, rhs) match {
    case (Precise(l), Precise(r))         => Size(l - r)
    case (Infinite, Finite(_, _))         => Infinite
    case (Finite(_, _), Infinite)         => Empty
    case (Finite(l1, h1), Finite(l2, h2)) => bounded(l1 - h2, h1 - l2)
    case (Bounded(l1, h1), rhs: Precise)  => bounded(l1 - rhs, h1 - rhs)
    case _                                => unknown
  }
  def min(rhs: Size): Size = (lhs, rhs) match {
    case (Infinite, _)                            => rhs
    case (_, Infinite)                            => lhs
    case (Precise(x), Precise(y))                 => if (x <= y) lhs else rhs
    case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 min l2, h1 min h2)
  }
  def max(rhs: Size): Size = (lhs, rhs) match {
    case (Infinite, _)                            => Infinite
    case (_, Infinite)                            => Infinite
    case (Precise(x), Precise(y))                 => if (x >= y) lhs else rhs
    case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 max l2, h1 max h2)
  }
}

final class FunOps[A, B](val f: Fun[A, B]) extends AnyVal {
  outer =>

  def get(x: A): Option[B]            = if (f isDefinedAt x) Some(f(x)) else None
  def getOr(key: A, alt: => B): B     = get(key) getOrElse alt
  def orElse(g: Fun[A, B]): Fun[A, B] = OrElse(f, g)
  def mapIn[C](g: C => A): Fun[C, B]  = AndThen(Fun(g), f)
  def mapOut[C](g: B => C): Fun[A, C] = AndThen(f, Fun(g))

  def defaulted(g: A => B): Defaulted[A, B] = f match {
    case Defaulted(_, u) => Defaulted(g, u)
    case _               => Defaulted(g, f)
  }

  def filterIn(p: A => Boolean): FilterIn[A, B] = f match {
    case FilterIn(p0, u) => FilterIn(x => p0(x) && p(x), u)
    case _               => FilterIn(p, f)
  }

  def traced(in: A => Unit, out: B => Unit): Fun[A, B] = ( f
    .   mapIn[A] { x => in(x) ; x }
    .  mapOut[B] { x => out(x) ; x }
  )
  def memoized: Fun[A, B] = {
    val cache = scala.collection.mutable.Map[A, B]()
    Opaque[A, B](x => cache.getOrElseUpdate(x, f(x))) filterIn (x => (cache contains x) || (f isDefinedAt x))
  }
}
