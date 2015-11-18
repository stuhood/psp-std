package psp
package std

import api._

object View {
  def apply[A, R](xs: R): AtomicView[A, R] = xs match {
    case xs: Array[A @unchecked]       => fromArray(xs).castTo
    case xs: Each[A @unchecked]        => each(xs)
    case xs: jMap[_, _]                => each(Each fromJavaMap xs).castTo
    case xs: sCollection[A @unchecked] => each(Each fromScala xs)
    case xs: jIterable[A @unchecked]   => each(Each fromJava xs)
    case _                             => abort(s"Not recognizable as a collection: $xs")
  }

  def fromScala[A, R <: sCollection[A]](xs: R): AtomicView[A, R] = each(Each fromScala xs)
  def fromJava[A, R <: jIterable[A]](xs: R): AtomicView[A, R]    = each(Each fromJava xs)
  def fromString(s: String): DirectView[Char, String]            = direct(Direct fromString s)
  def fromArray[A](xs: Array[A]): DirectView[A, Array[A]]        = direct(Direct fromArray xs)
  def each[A, Repr](xs: Each[A]): AtomicView[A, Repr]            = new LinearView(xs)
  def direct[A, Repr](xs: Direct[A]): DirectView[A, Repr]        = new DirectView(xs)
}
