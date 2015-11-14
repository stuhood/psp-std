package psp
package dev

import std._
import com.google.common.{ cache => guava }
import guava.ForwardingLoadingCache.SimpleForwardingLoadingCache

package cache {
  object Removal {
    val Collected = guava.RemovalCause.COLLECTED
    val Expired   = guava.RemovalCause.EXPIRED
    val Explicit  = guava.RemovalCause.EXPLICIT
    val Replaced  = guava.RemovalCause.REPLACED
    val Size      = guava.RemovalCause.SIZE
  }

  trait PspCache[K, V] extends guava.LoadingCache[K, V] with (K => V) {
    def update(key: K, value: V): this.type = {
      invalidate(key)
      get(key, callable(value))
      this
    }
  }
  final class PspCacheImpl[K, V](underlying: GuavaCache[K, V]) extends SimpleForwardingLoadingCache[K, V](underlying) with PspCache[K, V] {
    override def toString = stats.toString
  }
  class PspBuilder[K, V](load: K => V, builder: Builder[K, V]) {
    def build(): PspCache[K, V]                       = new PspCacheImpl(builder build GuavaCacheLoader(load))
    def config(f: BuilderFun[K, V]): PspBuilder[K, V] = new PspBuilder(load, f(builder))

    def valueWeighs(f: V => Int): PspBuilder[K, V]              = weighs((k, v) => f(v))
    def weighs(f: (K, V) => Int): PspBuilder[K, V]              = config(_ weigher GuavaWeigher(f))
    def maxWeight(max: Int): PspBuilder[K, V]                   = config(_ maximumWeight max)
    def onRemoval(f: (Removal, K, V) => Unit): PspBuilder[K, V] = config(_ removalListener GuavaRemovalListener(f))
  }
  class NewCache[K](spec: Option[BuilderSpec]) {
    def apply[V](load: K => V): PspBuilder[K, V] = new PspBuilder(load, spec map newBuilder[K, V] orEmpty)
  }
  final case class GuavaWeigher[K, V](f: (K, V) => Int) extends guava.Weigher[K, V] {
    def weigh(key: K, value: V): Int = f(key, value)
  }
  final case class GuavaCacheLoader[K, V](f: K => V) extends guava.CacheLoader[K, V] {
    def load(key: K): V = f(key)
  }
  final case class GuavaRemovalListener[K, V](f: (Removal, K, V) => Unit) extends guava.RemovalListener[K, V] {
    def onRemoval(event: Notification[K, V]): Unit = f(event.getCause, event.getKey, event.getValue)
  }
}

package object cache {
  type BuilderSpec        = guava.CacheBuilderSpec
  type Notification[K, V] = guava.RemovalNotification[K, V]
  type Removal            = guava.RemovalCause
  type Builder[K, V]      = guava.CacheBuilder[K, V]
  type GuavaCache[K, V]   = guava.LoadingCache[K, V]
  type RemovalFun[K, V]   = Removal ?=> ((K, V) => Unit)
  type BuilderFun[K, V]   = ToSelf[Builder[K, V]]

  def newBuilder[K, V](): Builder[K, V]                  = guava.CacheBuilder.newBuilder().castTo
  def newBuilder[K, V](spec: BuilderSpec): Builder[K, V] = guava.CacheBuilder from spec castTo
  def newCache[K] : NewCache[K]                          = new NewCache[K](None)
  def newSpec[K](spec: BuilderSpec): NewCache[K]         = new NewCache[K](Some(spec))

  implicit def newSpec(spec: String): BuilderSpec = guava.CacheBuilderSpec parse spec

  implicit class GuavaCacheOps[K, V](val cache: GuavaCache[K, V]) extends AnyVal {
    def apply(key: K): V = cache get key
  }
  implicit class GuavaCacheBuilderOps[K, V](val b: Builder[K, V]) extends AnyVal {
    def valueWeighs(f: V => Int): Builder[K, V]              = weighs((k, v) => f(v))
    def weighs(f: (K, V) => Int): Builder[K, V]              = b weigher GuavaWeigher(f)
    def maxWeight(max: Int): Builder[K, V]                   = b maximumWeight max
    def onRemoval(f: (Removal, K, V) => Unit): Builder[K, V] = b removalListener GuavaRemovalListener(f)
  }
  // TODO - enforce all this in a macro.
  //
  // concurrencyLevel=[integer]: sets CacheBuilder.concurrencyLevel.
  // initialCapacity=[integer]: sets CacheBuilder.initialCapacity.
  // maximumSize=[long]: sets CacheBuilder.maximumSize.
  // maximumWeight=[long]: sets CacheBuilder.maximumWeight.
  // expireAfterAccess=[duration]: sets CacheBuilder.expireAfterAccess(long, java.util.concurrent.TimeUnit).
  // expireAfterWrite=[duration]: sets CacheBuilder.expireAfterWrite(long, java.util.concurrent.TimeUnit).
  // refreshAfterWrite=[duration]: sets CacheBuilder.refreshAfterWrite(long, java.util.concurrent.TimeUnit).
  // weakKeys: sets CacheBuilder.weakKeys().
  // softValues: sets CacheBuilder.softValues().
  // weakValues: sets CacheBuilder.weakValues().
  // recordStats: sets CacheBuilder.recordStats().
  //
  // The string syntax is a series of comma-separated keys or key-value pairs, each corresponding to a CacheBuilder method.
  // Durations are represented by an integer, followed by one of "d", "h", "m", or "s", representing days, hours, minutes, or seconds respectively.
  // (There is currently no syntax to request expiration in milliseconds, microseconds, or nanoseconds.)
  // Whitespace before and after commas and equal signs is ignored.
  // Keys may not be repeated; it is also illegal to use the following pairs of keys in a single value:
  //     maximumSize and maximumWeight
  //     softValues and weakValues
  //
  // CacheBuilderSpec does not support configuring CacheBuilder methods with non-value parameters. These must be configured in code.
}
