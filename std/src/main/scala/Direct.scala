package psp
package std

import api._
import scala.collection.mutable.WrappedArray

object Direct {
  final val Empty: Direct[Nothing] = Pure(Precise(0), _ => sys error "<empty>")

  final case class WrapSeq[A](xs: scSeq[A]) extends AnyVal with DirectImpl[A] {
    def size: Precise                       = Precise(xs.size)
    def elemAt(i: Index): A                 = xs(i.getInt)
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }
  final case class WrapArray[A](xs: Array[_]) extends AnyVal with DirectImpl[A] {
    def size                                = Precise(xs.length)
    def elemAt(i: Index): A                 = xs(i.getInt).castTo[A]
    @inline def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elemAt(i)))
  }
  final case class WrapJava[A](xs: jList[A]) extends AnyVal with DirectImpl[A] {
    def size                                = Precise(xs.size)
    def elemAt(i: Index)                    = xs get i.getInt
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }
  final case class WrapString(xs: String) extends AnyVal with DirectImpl[Char] {
    def size                                   = Precise(xs.length)
    def elemAt(i: Index)                       = xs charAt i.getInt
    @inline def foreach(f: Char => Unit): Unit = size.indices foreach (i => f(xs elemAt i))
  }
  final case class Pure[A](size: Precise, elem: Index => A) extends DirectImpl[A] {
    def elemAt(i: Index)                    = elem(i)
    @inline def foreach(f: A => Unit): Unit = size.indices foreach (i => f(elem(i)))
  }

  trait DirectImpl[+A] extends Any with Direct[A] {
    def isEmpty = size.isZero
  }

  def fromScala[A](xs: sCollection[A]): Direct[A] = xs match {
    case xs: scSeq[A] => WrapSeq(xs)
    case _            => WrapSeq(xs.toSeq.seq)
  }

  def empty[A] : Direct[A]                             = Empty
  def fromJava[A](xs: jList[A]): Direct[A]             = WrapJava(xs)
  def fromString(xs: String): Direct[Char]             = WrapString(xs)
  def fromArray[A](xs: Array[A]): Direct[A]            = WrapArray[A](xs)
  def wrapArray[A](xs: Array[_]): Direct[A]            = WrapArray[A](xs)
  def pure[A](size: Precise, f: Index => A): Direct[A] = Pure(size, f)
  def reversed[A](xs: Direct[A]): Direct[A]            = Pure(xs.size, i => xs(xs.lastIndex - i.get))

  def apply[A](xs: A*): Direct[A] = xs match {
    case xs: WrappedArray[_] => fromArray[A](xs.array.castTo[Array[A]])
    case _                   => fromScala(xs)
  }
}
