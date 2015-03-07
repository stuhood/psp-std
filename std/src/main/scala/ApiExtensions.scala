package psp
package std
package ops

import api._

final class ShowableSeqOps[A: Show](xs: Each[A]) {
  private def strs: Each[String]       = xs map (_.to_s)
  private def nonEmpties: Each[String] = strs filterNot (_.isEmpty) map (_.trim)
  def inParens: String                 = "(" ~ joinComma ~ ")"
  def joinLines: String                = strs mkString EOL
  def joinComma: String                = nonEmpties mkString ", "
  def joinParents: String              = nonEmpties mkString " with "
  def joinWords: String                = nonEmpties mkString " "
}
final class IndexRangeOps(xs: IndexRange) {
  def *(n: Int): IndexRange = indexRange(xs.startInt * n, xs.endInt * n)
}

final class InMapOps[K, V](xs: InMap[K, V]) {
  def comap[K1](f: K1 => K): InMap[K1, V] = new InMap.Impl(xs.domain comap f, xs.lookup comap f)
  def partial: K ?=> V                    = newPartial(contains, xs.apply)
  def contains(key: K): Boolean           = xs domain key
  def ++(that: InMap[K, V]): InMap[K, V]  = new InMap.Impl(xs.domain union that.domain, xs.lookup orElse that.lookup)
}
final class ExMapOps[K, V](xs: ExMap[K, V]) {
  def ++(that: ExMap[K, V]): ExMap[K, V] = new ExMap.Impl(xs.domain union that.domain, xs.lookup orElse that.lookup)
}

final class InSetOps[A](xs: InSet[A]) {
  def comap[A1](f: A1 => A): InSet[A1]    = InSet(f andThen xs)
  def mapOnto[B](f: A => B): InMap[A, B]  = InMap(xs, f)
  def diff(that: InSet[A]): InSet[A]      = InSet.Diff(xs, that)
  def union(that: InSet[A]): InSet[A]     = InSet.Union(xs, that)
  def intersect(that: InSet[A]): InSet[A] = InSet.Intersect(xs, that)
  def complement: InSet[A] = xs match {
    case InSet.Complement(xs) => xs
    case _                             => InSet.Complement(xs)
  }

  def filter(p: Predicate[A]): InSet[A]    = this intersect inSet(p)
  def filterNot(p: Predicate[A]): InSet[A] = this filter !p
}
final class ExSetOps[A](xs: ExSet[A]) {
  private implicit def heq: HashEq[A] = xs.hashEq

  /** replace adds the element, ejecting any existing element which measures === to it.
   *  add adds the element only if no existing element is === to it.
   */

  def add(x: A): ExSet[A]                 = if (xs(x)) xs else xs union exSet(x)
  def canonicalize(x: A): A               = xs.findOr(_ === x, x)
  def diff(that: ExSet[A]): ExSet[A]      = ExSet.Diff(xs, that)
  def filter(p: Predicate[A]): ExSet[A]   = ExSet.Filtered(xs, p)
  def intersect(that: ExSet[A]): ExSet[A] = ExSet.Intersect(xs, that)
  def isSubsetOf(ys: InSet[A]): Boolean   = xs forall ys
  def mapOnto[B](f: A => B): ExMap[A, B]  = new ExMap.Impl(xs, Lookup total f)
  def replace(x: A): ExSet[A]             = if (xs(x)) without(x) add x else this add x
  def reverse: ExSet[A]                   = xs // XXX
  def union(that: ExSet[A]): ExSet[A]     = ExSet.Union(xs, that)
  def without(x: A): ExSet[A]             = xs diff exSet(x)

  def filterNot(p: Predicate[A]): ExSet[A] = this filter !p
}

trait HasPreciseSizeMethods extends Any {
  def size: Precise

  def longSize: Long      = size.value
  def intSize: Int        = longSize.safeInt
  def isZero: Boolean     = longSize == 0L
  def isPositive: Boolean = longSize > 0L
  def indices: IndexRange = indexRange(0, intSize)
  def nths: Direct[Nth]   = indices mapNow (_.toNth)
  def lastIndex: Index    = Index(longSize - 1)  // effectively maps both undefined and zero to no index.
  def lastNth: Nth        = lastIndex.toNth

  def containsIndex(index: Index): Boolean            = indices contains index
  @inline def mapIndices[A](f: Index => A): Direct[A] = indices mapNow f
  @inline def foreachIndex(f: Index => Unit): Unit    = indices foreach f
  @inline def foreachNth(f: Nth => Unit): Unit        = indices foreach (i => f(i.toNth))
}

final class HasPreciseSizeOps(val x: HasPreciseSize) extends HasPreciseSizeMethods {
  def size: Precise = x.size
}

final class PreciseOps(val size: Precise) extends AnyRef with HasPreciseSizeMethods {
  def get: Long        = longSize
  def safeInt          = get.safeInt
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
  def padLeft(s: String, ch: Char): String = if (s.length >= longSize) s else (this - s.length timesConst ch mkString "") ~ s

  def leftFormatString  = if (size.isZero) "%s" else "%%-%ds" format intSize
  def rightFormatString = if (size.isZero) "%s" else "%%%ds" format intSize

  override def toString = s"$longSize"
}

final class BooleanAlgebraOps[A](val algebra: BooleanAlgebra[A]) extends AnyVal {
  def map[B](f: B => A, g: A => B): BooleanAlgebra[B] = new Algebras.Mapped[A, B](algebra, f, g)
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
