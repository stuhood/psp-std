package psp
package std

import api._

object Direct {
  final val Empty: Direct[Nothing] = Pure(Precise(0), _ => sys error "<empty>")

  final case class WrapVector[A](xs: sciVector[A]) extends AnyVal with DirectImpl[A] {
    def +:(x: A): WrapVector[A]             = WrapVector(x +: xs)
    def :+(x: A): WrapVector[A]             = WrapVector(xs :+ x)
    def ++(ys: Direct[A]): WrapVector[A]    = WrapVector(xs ++ ys.seq)
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

  def empty[A] : Direct[A]                             = Empty
  def fromScala[A](xs: sCollection[A]): Direct[A]      = WrapVector(xs.toVector)
  def fromJava[A](xs: jList[A]): Direct[A]             = WrapJava(xs)
  def fromString(xs: String): Direct[Char]             = WrapString(xs)
  def fromArray[A](xs: Array[A]): Direct[A]            = WrapArray[A](xs)
  def wrapArray[A](xs: Array[_]): Direct[A]            = WrapArray[A](xs)
  def pure[A](size: Precise, f: Index => A): Direct[A] = Pure(size, f)
  def apply[A](xs: A*): Direct[A]                      = xs match {
    case xs: scmWrappedArray[_] => fromArray[A](xs.array.castTo[Array[A]])
    case _                      => fromScala(xs.toVector)
  }
  def join[A](xs: Direct[A], ys: Direct[A]): Direct[A] = xs match {
    case xs: WrapVector[A] => xs ++ ys
    case _                 => WrapVector(xs.toScalaVector) ++ ys
  }
  def append[A](xs: Direct[A], x: A): Direct[A] = xs match {
    case xs: WrapVector[A] => xs :+ x
    case _                 => WrapVector(xs.toScalaVector) :+ x
  }
  def prepend[A](x: A, xs: Direct[A]): Direct[A] = xs match {
    case xs: WrapVector[A] => x +: xs
    case _                 => x +: WrapVector(xs.toScalaVector)
  }
}
