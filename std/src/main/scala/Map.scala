package psp
package std

import api._

object Fun {
  private val Undefined: Opaque[Any, Nothing] = Opaque[Any, Nothing](x => throw new java.lang.IllegalArgumentException("" + x))
  private val Empty: FilterIn[Any, Nothing]   = FilterIn[Any, Nothing](_ => false, Undefined)

  def empty[A, B] : Fun[A, B]                     = Empty
  def apply[A, B](f: A => B): Opaque[A, B]        = Opaque(f)
  def partial[A, B](pf: A ?=> B): FilterIn[A, B]  = FilterIn(pf.isDefinedAt, Opaque(pf))
  def const[B](value: B): Opaque[Any, B]          = Opaque(_ => value)
  def finite[A, B](kvs: (A->B)*): FiniteDom[A, B] = FiniteDom(kvs map fst toEqualsSet, partial(kvs map (x => x._1 -> x._2) toMap))
  def orElse[A, B](fs: Fun[A, B]*): Fun[A, B]     = if (fs.isEmpty) empty else fs reduceLeft (OrElse(_, _))
}

object ExMap {
  def empty[K, V] : ExMap[K, V]                                   = apply(Fun.finite())
  def apply[K, V](f: FiniteDom[K, V]): ExMap[K, V]                = new Impl(f)
  def apply[K, V](keys: ExSet[K], lookup: Fun[K, V]): ExMap[K, V] = apply(FiniteDom(keys, lookup))
  def fromJava[K, V](xs: jMap[K, V]): ExMap[K, V]                 = fromScala(xs.m.toScalaMap)
  def fromScala[K, V](xs: scMap[K, V]): ExMap[K, V]               = apply(xs.keys.toEqualsSet, Opaque(xs))

  def impl[K, V](xs: ExMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.lookup)
  }

  def unapplySeq[K, V](map: ExMap[K, V]): Some[scSeq[K]] = Some(map.keyVector.seq)

  final class Impl[K, V](val lookup: FiniteDom[K, V]) extends ExMap[K, V] {
    import lookup._

    def apply(key: K): V                      = lookup(key)
    def map[V1](g: V => V1): ExMap[K, V1]     = ExMap(keys, f mapOut g)
    def filterKeys(p: ToBool[K]): ExMap[K, V] = ExMap(keys filter p, f)
  }
}
object InMap {
  def empty[K, V] : InMap[K, V]              = apply(Fun.empty)
  def apply[K, V](f: Fun[K, V]): InMap[K, V] = new Impl(f)
  def impl[K, V](xs: InMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.lookup)
  }

  final class Impl[K, V](val lookup: Fun[K, V]) extends InMap[K, V] {
    def apply(key: K): V                      = lookup(key)
    def map[V1](f: V => V1): InMap[K, V1]     = InMap(lookup mapOut f)
    def filterKeys(p: ToBool[K]): InMap[K, V] = InMap(lookup filterIn p)
  }
}
