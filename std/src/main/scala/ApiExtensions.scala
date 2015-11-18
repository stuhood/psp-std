package psp
package std
package ops

import api._, StdEq._

final class DocSeqOps(xs: Direct[Doc]) {
  private def strs: Direct[String]       = xs mapNow (_.render)
  private def nonEmpties: Direct[String] = strs filterNot (_.isEmpty) mapNow (_.trim)

  def inParens: String    = "(" append joinComma append ")"
  def joinLines: String   = strs mkString EOL
  def joinComma: String   = nonEmpties mkString ", "
  def joinParents: String = nonEmpties mkString " with "
  def joinWords: String   = nonEmpties mkString " "
}
final class ExMapOps[K, V](xs: ExMap[K, V]) {
  type Entry = K -> V

  def keys: View[K]             = keySet.m
  def values: View[V]           = keyVector map xs.lookup
  def keySet: ExSet[K]          = xs.lookup.keys
  def keyVector: Vec[K]         = keys.toVec
  def entries: View[Entry]      = keyVector mapZip xs.lookup
  def contains(key: K): Boolean = keySet(key)
  def size: Size                = keys.size

  def filterValues(p: ToBool[V]): ExMap[K, V] = xs filterKeys (k => p(xs(k)))
  def +(entry: Entry): ExMap[K, V]            = ExMap(keySet + entry._1, Fun finite entry orElse xs.lookup)
}
final class ExSetOps[A](xs: ExSet[A]) {
  def add(x: A): ExSet[A]                = xs + x
  def filter(p: ToBool[A]): ExSet[A]     = ExSet.Filtered(xs, p)
  def union(that: ExSet[A]): ExSet[A]    = ExSet.Union(xs, that)
  def mapOnto[B](f: A => B): ExMap[A, B] = ExMap(xs, Fun(f))
}

trait HasPreciseSizeMethods extends Any {
  def size: Precise

  def longSize: Long       = size.longValue
  def intSize: Int         = size.intValue
  def isZero: Boolean      = longSize == 0L
  def isPositive: Boolean  = longSize > 0L
  def indices: IndexRange  = indexRange(0, intSize)
  def nths: Direct[Nth]    = indices mapNow (_.toNth)
  def lastIntIndex: Int    = lastIndex.getInt
  def lastIndex: Index     = Index(longSize - 1)  // effectively maps both undefined and zero to no index.
  def lastNth: Nth         = lastIndex.toNth

  def containsIndex(index: Index): Boolean = indices.m contains index

  @inline def foreachIndex(f: Index => Unit): Unit     = if (isPositive) lowlevel.ll.foreachConsecutive(0, lastIntIndex, i => f(Index(i)))
  @inline def foreachIntIndex(f: Int => Unit): Unit    = if (isPositive) lowlevel.ll.foreachConsecutive(0, lastIntIndex, f)
  @inline def mapIndices[A](f: Index => A): Direct[A]  = indices mapNow f
}

final class HasPreciseSizeOps(val x: HasPreciseSize) extends HasPreciseSizeMethods {
  def size: Precise = x.size
}

final class PreciseOps(val size: Precise) extends AnyRef with HasPreciseSizeMethods {
  def get: Long        = size.longValue
  def getInt: Int      = size.intValue
  def toDouble: Double = get.toDouble

  def + (n: Int): Precise = (longSize + n).size
  def - (n: Int): Precise = (longSize - n).size
  def * (n: Int): Precise = (longSize * n).size
  def / (n: Int): Precise = (longSize / n).size
  def % (n: Int): Precise = (longSize % n).size

  def /+ (n: Int): Precise = (this / n) + ( if ((this % n).isZero) 0 else 1 )

  def + (n: Precise): Precise = (longSize + n.longSize).size
  def - (n: Precise): Precise = (longSize - n.longSize).size
  def * (n: Precise): Precise = (longSize * n.longSize).size
  def / (n: Precise): Precise = (longSize / n.longSize).size
  def % (n: Precise): Precise = (longSize % n.longSize).size

  def min(that: Precise): Precise = (longSize min that.longSize).size
  def max(that: Precise): Precise = (longSize max that.longSize).size
  def increment: Precise          = (longSize + 1L).size
  def decrement: Precise          = (longSize - 1L).size

  def timesConst[A](elem: A): Each[A]   = Each const elem take size
  def timesEval[A](body: => A): Each[A] = Each continually body take size

  def toIntRange                           = intRange(0, intSize)
  def padLeft(s: String, ch: Char): String = if (s.length >= longSize) s else (this - s.length timesConst ch mkString "") append s

  def leftFormatString  = if (size.isZero) "%s" else "%%-%ds" format intSize
  def rightFormatString = if (size.isZero) "%s" else "%%%ds" format intSize

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

  def isNonZero    = !loBound.isZero
  def isZero       = lhs match {
    case Precise(0L) => true
    case _           => false
  }
  def isFinite         = lhs.hiBound !== Infinite
  def atLeast: Size    = bounded(lhs, Infinite)
  def atMost: Size     = bounded(Empty, lhs)
  def upTo(hi: Atomic) = bounded(lhs, hi)
  def hiBound: Atomic = lhs match {
    case Bounded(_, hi) => hi
    case x: Atomic      => x
  }
  def loBound: Atomic = lhs match {
    case Bounded(lo, _) => lo
    case x: Atomic      => x
  }

  def mapAtomic(f: Precise => Precise, g: Atomic => Atomic): Size = lhs match {
    case x: Atomic       => g(x)
    case Bounded(lo, hi) => bounded(f(lo), g(hi))
  }

  def slice(range: IndexRange): Size = (this - range.toDrop) min range.toTake

  /** For instance taking the union of two sets. The new size is
   *  at least the size of the larger operand, but at most the sum
   *  of the two sizes.
   */
  def union(rhs: Size): Size     = bounded(lhs max rhs, lhs + rhs)
  def intersect(rhs: Size): Size = bounded(0.size, lhs min rhs)
  def diff(rhs: Size): Size      = bounded(lhs - rhs, lhs)

  def * (m: Long): Size = lhs match {
    case Precise(n)                        => n * m size
    case Bounded(Precise(lo), Precise(hi)) => bounded(lo * m size, hi * m size)
    case Bounded(Precise(lo), Infinite)    => if (m == 0L) unknown else bounded(lo * m size, Infinite)
    case Infinite                          => if (m == 0L) unknown else Infinite
  }
  def * (rhs: Size): Size = lhs match {
    case Precise(n)                        => this * n
    case Bounded(Precise(lo), Precise(hi)) => bounded(rhs * lo, rhs * hi)
    case Bounded(Precise(lo), Infinite)    => if (rhs.isZero) unknown else bounded(rhs * lo, Infinite)
    case Infinite                          => if (rhs.isZero) unknown else Infinite
  }

  def + (rhs: Size): Size = (lhs, rhs) match {
    case (Infinite, _) | (_, Infinite)            => Infinite
    case (Precise(l), Precise(r))                 => l + r size
    case (GenBounded(l1, h1), GenBounded(l2, h2)) => bounded(l1 + l2, h1 + h2)
  }
  def - (rhs: Size): Size = (lhs, rhs) match {
    case (Precise(l), Precise(r))         => l - r size
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

  def opaquely: Opaque[A, B] = f match {
    case x: Opaque[_, _] => x
    case _               => Opaque(x => if (f isDefinedAt x) f(x) else abort("" + x))
  }
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
     mapIn[A] { x => in(x) ; x }
    mapOut[B] { x => out(x) ; x }
  )
  def memoized: Fun[A, B] = {
    val cache = scala.collection.mutable.Map[A, B]()
    Opaque[A, B](x => cache.getOrElseUpdate(x, f(x))) filterIn (x => (cache contains x) || (f isDefinedAt x))
  }
}
