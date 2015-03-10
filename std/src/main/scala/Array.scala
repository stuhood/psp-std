package psp
package std

import api._

/** A basic specialized immutable Array. The apply copies everything in so nobody
 *  can get too clever on us.
 */
final class ImmutableArray[@spec A] private[std] (xs: Array[A]) extends Direct.DirectImpl[A] {
  val size: Precise                                 = Precise(xs.length)
  def elemAt(i: Index): A                           = xs(i)
  def foreach(f: A => Unit): Unit                   = xs foreach f
  def mapNow[B: CTag](f: A => B): ImmutableArray[B] = new ImmutableArray[B](xs mapNow f)
}
/** An immutable Array where the known element type may not match the low-level type.
 *  Only reference Arrays are allowed.
 */
final class ImmutableWrappedArray[A] private[std] (xs: Array[_ <: AnyRef]) extends Direct.DirectImpl[A] {
  val size: Precise                                  = Precise(xs.length)
  def elemAt(i: Index): A                            = xs(i).castTo[A]
  def foreach(f: A => Unit): Unit                    = this.foreachIndex(i => f(elemAt(i)))
  def mapNow[B](f: A => B): ImmutableWrappedArray[B] = new ImmutableWrappedArray[B](this map f toRefArray)
}

object ImmutableArray {
  def apply[@spec A : CTag](xs: A*): ImmutableArray[A] = new ImmutableArray[A](xs.toArray)
  def wrap[A](xs: A*): ImmutableWrappedArray[A]        = new ImmutableWrappedArray[A](xs.toRefArray)
}
