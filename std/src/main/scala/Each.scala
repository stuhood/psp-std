package psp
package std

import api._, StdEq._

sealed abstract class CollectionSizeException(msg: String) extends RuntimeException(msg)
final class InfiniteSizeException(msg: String) extends CollectionSizeException(msg)
final class LongSizeException(msg: String) extends CollectionSizeException(msg)

object Each {
  final case class WrapJavaMap[K, V](xs: jMap[K, V]) extends AnyVal with Each[K -> V] {
    def size = xs.size
    @inline def foreach(f: ToUnit[K -> V]): Unit = xs.keySet foreach (k => f(k -> (xs get k)))
  }
  final case class WrapJava[A](xs: jIterable[A]) extends AnyVal with Each[A] {
    def size = xs match {
      case xs: jCollection[_] => xs.size
      case _                  => api.Size.Unknown
    }
    @inline def foreach(f: A => Unit): Unit = xs.iterator foreach f
  }
  final case class WrapScala[A](xs: sCollection[A]) extends AnyVal with Each[A] {
    def size: Size = if (xs.hasDefiniteSize) xs.size else api.Size.Unknown
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }

  final class ToScalaTrav[A](xs: Foreach[A]) extends sciTraversable[A] {
    def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }

  /** We have to produce a scala Seq in order to return from an extractor.
   *  That requires us to produce made-up values for these methods thanks to
   *  scala's rampant overspecification.
   */
  final class ToScalaSeq[A](xs: Each[A]) extends sciSeq[A] {
    override def length: Int                 = xs.size.getInt
    def iterator: scIterator[A]              = xs.iterator
    def apply(index: Int): A                 = xs drop index.size head
    override def foreach[U](f: A => U): Unit = xs foreach (x => f(x))
  }
  final class Impl[A](val size: Size, mf: Suspended[A]) extends Each[A] {
    @inline def foreach(f: A => Unit): Unit = mf(f)
  }
  final case class Joined[A](xs: Each[A], ys: Each[A]) extends Each[A] {
    def size = xs.size + ys.size
    @inline def foreach(f: A => Unit): Unit = {
      xs foreach f
      ys foreach f
    }
  }
  trait AtomicSize[+A] extends Any with Each[A] with HasAtomicSize
  trait InfiniteSize[+A] extends Any with AtomicSize[A] {
    def isEmpty  = false
    def size = Infinite
  }
  object KnownSize {
    def unapply[A](xs: Each[A]) = xs.size matchOpt { case x: Atomic => x }
  }

  final case class Sized[A](underlying: Each[A], override val size: Precise) extends Each[A] with HasPreciseSize {

    // implicitly[Order[Precise]]

    def isEmpty = size.isZero
    @inline def foreach(f: A => Unit): Unit = {
      var count = Size(0)
      underlying foreach { x =>
        if (count >= size) return
        f(x)
        count += 1
      }
    }
  }
  final case class Constant[A](elem: A) extends AnyVal with InfiniteSize[A] {
    @inline def foreach(f: A => Unit): Unit = while (true) f(elem)
  }
  final case class Continually[A](fn: () => A) extends AnyVal with InfiniteSize[A] {
    @inline def foreach(f: A => Unit): Unit = while (true) f(fn())
  }
  final case class Unfold[A](zero: A)(next: ToSelf[A]) extends InfiniteSize[A] {
    @inline def foreach(f: A => Unit): Unit = {
      var current = zero
      while (true) { f(current) ; current = next(current) }
    }
  }

  def indices: Indexed[Index] = Indexed.indices

  def apply[A](mf: Suspended[A]): Each[A]                    = new Impl[A](impl.Size.Unknown, mf)
  def const[A](elem: A): Constant[A]                         = Constant[A](elem)
  def continuallyWhile[A](p: ToBool[A])(expr: => A): Each[A] = continually(expr) takeWhile p
  def continually[A](elem: => A): Continually[A]             = Continually[A](() => elem)
  def elems[A](xs: A*): Each[A]                              = apply[A](xs foreach _)
  def empty[A] : Each[A]                                     = Direct.Empty
  def fromJavaMap[K, V](xs: jMap[K, V]): Each[K -> V]        = WrapJavaMap(xs)
  def fromJava[A](xs: jIterable[A]): Each[A]                 = WrapJava(xs)
  def join[A](xs: Each[A], ys: Each[A]): Each[A]             = Joined[A](xs, ys)
  def unfold[A](start: A)(next: ToSelf[A]): Unfold[A]        = Unfold[A](start)(next)

  /** Memoizes iterators. */
  def fromScala[A](xs: GTOnce[A]): Each[A] = xs match {
    case xs: sCollection[_] => WrapScala(xs)
    case _                  => WrapScala(xs.to[sciStream])
  }

  def unapplySeq[A](xs: Each[A]): Some[scSeq[A]] = Some(xs.seq)
  def unapplySeq[A](xs: View[A]): Some[scSeq[A]] = Some(xs.seq)

  def show[A: Show](xs: Each[A], minElements: Precise, maxElements: Precise): String = xs splitAt maxElements.lastIndex match {
    case Split(xs, ys) if ys.isEmpty => xs mk_s ", "
    case Split(xs, _)                => (xs take minElements mk_s ", ") append ", ..."
  }
}
