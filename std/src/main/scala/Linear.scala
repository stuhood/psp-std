package psp
package std

import api._

object Linear {
  final val Empty: Linear[Nothing] = WrapList(Nil)

  trait LinearImpl[A] extends Any with Linear[A] {
    type This <: LinearImpl[A]
    def ::(x: A): This
  }

  final case class Join[A](left: Linear[A], right: Linear[A]) extends LinearImpl[A] {
    type This          = Join[A]
    def ::(x: A): This = Join(x :: left, right)

    def isEmpty         = false
    def size: Size      = impl.Size(left) + impl.Size(right)
    def head: A         = left.head
    def tail: Linear[A] = left.tail |> (xs => if (xs.isEmpty) right else Join(xs, right))

    @inline def foreach(f: A => Unit): Unit = {
      left foreach f
      right foreach f
    }
  }

  final case class WrapStream[A](xs: sciStream[A]) extends AnyVal with LinearImpl[A] {
    type This = WrapStream[A]
    def ::(x: A): This = WrapStream(sciStream.cons(x, xs))

    def isEmpty                 = xs.isEmpty
    def size: Size              = impl.Size(xs)
    def head: A                 = xs.head
    def tail: WrapStream[A]     = WrapStream(xs.tail)

    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }

  final case class WrapList[A](xs: sciList[A]) extends AnyVal with LinearImpl[A] {
    type This = WrapList[A]
    def ::(x: A): This = WrapList(x :: xs)

    def isEmpty           = xs.isEmpty
    def size: Size        = impl.Size(xs)
    def head: A           = xs.head
    def tail: WrapList[A] = WrapList(xs.tail)
    @inline def foreach(f: A => Unit): Unit = xs foreach f
  }

  def fromScala[A](xs: sCollection[A]): Linear[A] = xs match {
    case xs: sciStream[A] => WrapStream(xs)
    case _                => WrapList(xs.toList)
  }
  def fromJava[A](xs: jIterable[A]): Linear[A] = WrapList(xs.m.toScalaList)
  def empty[A] : Linear[A]                     = Empty
  def fill[A](n: Int)(body: => A): Linear[A]   = WrapList[A](sciList.fill(n)(body))
  def apply[A](xs: A*): Linear[A]              = WrapList[A](xs.toList)
  def cons[A](x: A, xs: Linear[A]): Linear[A]  = xs match {
    case xs: LinearImpl[A] => x :: xs
    case _                 => WrapList(x :: xs.toScalaList)
  }

  def join[A](left: Linear[A], right: Linear[A]): Linear[A] = (
    if (left.isEmpty) right
    else if (right.isEmpty) left
    else Join(left, right)
  )

  def unapplySeq[A](xs: Linear[A]): Some[scSeq[A]] = Some(xs.seq)
}
