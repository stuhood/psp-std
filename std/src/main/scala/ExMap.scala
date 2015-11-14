package psp
package std

import api._

object ExMap {
  def apply[K, V](keys: ExSet[K], pf: K ?=> V): ExMap[K, V] = new Impl(keys, Fun.partial(pf)) //  Lookup(pf))
  def fromJava[K, V](xs: jMap[K, V]): ExMap[K, V]           = new Impl(xs.keySet.toEqualsSet, Fun(xs get _)) //  Lookup.total(xs get _))
  def fromScala[K, V](xs: scMap[K, V]): ExMap[K, V]         = xs.toMap |> (xs => new Impl(xs.keys.toEqualsSet, Fun.partial(xs))) // Lookup(xs)))

  def unapplySeq[K, V](map: ExMap[K, V]): Some[sciSeq[K]] = Some(map.keyVector.toScalaSeq)
  def impl[K, V](xs: ExMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.domain, xs.lookup)
  }

  final class Impl[K, V](domain: ExSet[K], lookup: Fun[K, V]) extends InMap.InMapImpl[ExSet, K, V](domain, lookup) with ExMap[K, V] {
    type Entry          = K -> V
    type NewMap[K1, V1] = ExMap[K1, V1]

    protected[this] def newDomain(p: ToBool[K])                                                 = domain filter p
    protected[this] def newMap[K1, V1](domain: ExSet[K1], lookup: Fun[K1, V1]): Impl[K1, V1] = new Impl(domain, lookup)

    def +(key: K, value: V): This             = newMap(domain + key, Fun.literal(key -> value) orElse lookup)
    def entries: View[Entry]                  = keys mapZip (x => lookup(x))
    def foreach(f: (K -> V) => Unit): Unit    = foreachKey(k => f(k -> apply(k)))
    def foreachEntry(f: (K, V) => Unit): Unit = foreachKey(k => f(k, apply(k)))
    def foreachKey(f: K => Unit): Unit        = keys foreach f
    def isEmpty: Boolean                      = domain.isEmpty
    def iterator: scIterator[Entry]           = keysIterator map (k => (k, lookup(k)))
    def keyVector: Direct[K]                  = keys.toDirect
    def keys: View[K]                         = domain
    def keysIterator: scIterator[K]           = keys.iterator
    def seq: scSeq[Entry]                     = entries.seq
    def size: Precise                         = keyVector.size
    def values: View[V]                       = keys map (x => lookup(x))
    def valuesIterator: scIterator[V]         = keysIterator map (x => lookup(x))
  }
}
object InMap {
  def apply[K, V](keys: InSet[K], f: K => V): InMap[K, V] = new Impl(keys, Fun(f))
  def impl[K, V](xs: InMap[K, V]): Impl[K, V] = xs match {
    case xs: Impl[K, V] => xs
    case _              => new Impl(xs.domain, xs.lookup)
  }

  sealed abstract class InMapImpl[Domain[X] <: InSet[X], K, V](val domain: Domain[K], val lookup: Fun[K, V]) extends InMap[K, V] {
    type NewMap[K1, V1] <: InMap[K1, V1]
    type This = NewMap[K, V]

    protected[this] def newDomain(p: ToBool[K]): Domain[K]
    protected[this] def newMap[K1, V1](domain: Domain[K1], lookup: Fun[K1, V1]): NewMap[K1, V1]

    def apply(key: K): V                     = lookup.apply(key)
    def filterKeys(p: ToBool[K]): This       = newMap(newDomain(p), lookup)
    def filterValues(p: ToBool[V]): This     = filterKeys(k => p(apply(k)))
    def get(key: K): Option[V]               = lookup get key
    def getOr(key: K, alt: => V): V          = lookup.getOr(key, alt)
    def map[V1](f: V => V1): NewMap[K, V1]   = newMap(domain, lookup mapOut f)
    def withDefaultFunction(f: K => V): This = newMap(domain, lookup defaulted f)
    def withDefaultValue(v: V): This         = newMap(domain, lookup defaulted (_ => v))
  }

  final class Impl[K, V](domain: InSet[K], lookup: Fun[K, V]) extends InMapImpl[InSet, K, V](domain, lookup) with InMap[K, V] {
    type NewMap[K1, V1] = InMap[K1, V1]

    protected[this] def newDomain(p: ToBool[K])                                                 = domain filter p
    protected[this] def newMap[K1, V1](domain: InSet[K1], lookup: Fun[K1, V1]): Impl[K1, V1] = new Impl(domain, lookup)
  }
}
