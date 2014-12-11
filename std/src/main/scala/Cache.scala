package psp
package std

import api._
import com.google.common.{ cache => guava }
import guava.ForwardingLoadingCache.SimpleForwardingLoadingCache
import StdZero._

package cache {
  trait PspCache[K, V] extends guava.LoadingCache[K, V] with (K => V)

  final class PspCacheImpl[K, V](underlying: GuavaCache[K, V]) extends SimpleForwardingLoadingCache[K, V](underlying) with PspCache[K, V] {
    override def toString = stats.toString
  }
  class PspBuilder[K, V](load: K => V, fs: Direct[BuilderFun[K, V]]) {
    def build(): PspCache[K, V]                       = new PspCacheImpl(fs zfoldl ((b: Builder[K, V], f) => f(b)) build GuavaCacheLoader(load))
    def config(g: BuilderFun[K, V]): PspBuilder[K, V] = new PspBuilder(load, fs :+ g)
  }
  class NewCache[K]() {
    def apply[V](load: K => V): PspBuilder[K, V] = new PspBuilder(load, direct())
  }
  final case class GuavaWeigher[K, V](f: (K, V) => Int) extends guava.Weigher[K, V] {
    def weigh(key: K, value: V): Int = f(key, value)
  }
  final case class GuavaCacheLoader[K, V](f: K => V) extends guava.CacheLoader[K, V] {
    def load(key: K): V = f(key)
  }
}

package object cache {
  type Builder[K, V]    = guava.CacheBuilder[K, V]
  type GuavaCache[K, V] = guava.LoadingCache[K, V]
  type BuilderFun[K, V] = Unary[Builder[K, V]]

  def newBuilder[K, V](): Builder[K, V] = guava.CacheBuilder.newBuilder().castTo
  def newCache[K] : NewCache[K]         = new NewCache[K]

  implicit class GuavaCacheOps[K, V](val cache: GuavaCache[K, V]) extends AnyVal {
    def apply(key: K): V = cache get key
  }
  implicit class GuavaCacheBuilderOps[K, V](val b: Builder[K, V]) extends AnyVal {
    def valueWeighs(f: V => Int): Builder[K, V] = weighs((k, v) => f(v))
    def weighs(f: (K, V) => Int): Builder[K, V] = b weigher GuavaWeigher(f)
    def maxWeight(max: Int): Builder[K, V]      = b maximumWeight max
  }
}
