package psp
package std

import api._

sealed abstract class Plist[A] extends Linear[A] {
  def ::(hd: A): Plist[A] = Pcons(hd, this)

  def isEmpty = this eq Pnil
  def size = if (isEmpty) Precise(0) else Precise(1).atLeast
  @inline final def foreach(f: A => Unit): Unit = {
    def loop(xs: Plist[A]): Unit = xs match {
      case Pcons(hd, tl) => f(hd) ; loop(tl)
      case _             =>
    }
    loop(this)
  }
}
final case class Pcons[A](head: A, tail: Plist[A]) extends Plist[A]
final case object Pnil extends Plist[Nothing] {
  def head = abort("Pnil.head")
  def tail = abort("Pnil.tail")
}

object Plist {
  def empty[A] : Plist[A]        = Pnil.castTo[Plist[A]]
  def newBuilder[A] : Builder[A] = new Builder[A]()
  def apply[A](xs: A*): Plist[A] = xs.m.zfoldr[Plist[A]](_ :: _)

  final class Builder[A] extends Builds[A, Plist[A]] {
    def build(xs: Each[A]): Plist[A] = xs.zfoldr[Plist[A]](_ :: _)
  }
}
