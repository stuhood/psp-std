package psp
package std

import api._

object View {
  def apply[CC[X], A](xs: CC[A]): AnyView[A] = xs match {
    case xs: Array[A @unchecked]       => apply[A](Direct fromArray xs)
    case xs: Each[A @unchecked]        => apply[A](xs)
    case xs: sCollection[A @unchecked] => fromScala[A](xs)
    case xs: jIterable[A @unchecked]   => fromJava[A](xs)
    case _                             => abort(s"Not recognizable as a collection: $xs")
  }
  // def apply[A](xs: Array[A]): AnyView[A] = apply(Direct fromArray xs)
  def apply[A](xs: Each[A]): AnyView[A] = xs match {
    case xs: Direct[A] => direct(xs)
    case xs: Linear[A] => linear(xs)
    case _             => each(xs)
  }

  def fromString(s: String): AnyView[Char] = apply(Direct fromString s)
  def fromScala[A](xs: sCollection[A]): AnyView[A] = xs match {
    case xs: sciIndexedSeq[_] => Direct fromScala xs m
    case xs: sciLinearSeq[_]  => Linear fromScala xs m
    case xs: sciSet[_]        => ExSet fromScala xs m
    case _                    => Each fromScala xs m
  }
  def fromJava[A](xs: jIterable[A]): AnyView[A] = xs match {
    case xs: jList[_] => Direct fromJava xs m
    case xs: jSet[_]  => ExSet fromJava xs m
    case xs           => Each fromJava xs m
  }

  def each[A, Repr](xs: Each[A]): LinearView[A, Repr]     = new LinearView(xs)
  def linear[A, Repr](xs: Linear[A]): LinearView[A, Repr] = new LinearView(xs)
  def direct[A, Repr](xs: Direct[A]): DirectView[A, Repr] = new DirectView(xs)
}
