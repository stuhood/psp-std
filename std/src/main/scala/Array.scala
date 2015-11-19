package psp
package std

import api._
import Api.SpecTypes

/** A basic specialized immutable Array. The apply copies everything in so nobody
 *  can get too clever on us.
 */
final class ImmutableArray[@spec(SpecTypes) A] private[std] (xs: Array[A]) extends Direct.DirectImpl[A] {
  val size: Precise                                                  = Size(xs.length)
  def elemAt(i: Index): A                                            = xs(i)
  def foreach(f: A => Unit): Unit                                    = xs foreach f
  def mapNow[@spec(SpecTypes) B: CTag](f: A => B): ImmutableArray[B] = new ImmutableArray[B](xs mapNow f)
}
/** An immutable Array where the known element type may not match the low-level type.
 *  Only reference Arrays are allowed.
 */
final class ImmutableWrappedArray[A] private[std] (xs: Array[_ <: AnyRef]) extends Direct.DirectImpl[A] {
  val size: Precise                                  = Size(xs.length)
  def elemAt(i: Index): A                            = xs(i).castTo[A]
  def foreach(f: A => Unit): Unit                    = xs foreach (x => f(x.asInstanceOf[A]))
  def mapNow[B](f: A => B): ImmutableWrappedArray[B] = new ImmutableWrappedArray[B](this map f toRefArray)
}

object ImmutableArray {
  def apply[@spec(SpecTypes) A : CTag](xs: A*): ImmutableArray[A] = new ImmutableArray[A](xs.toArray)
  def wrap[A](xs: A*): ImmutableWrappedArray[A]                   = new ImmutableWrappedArray[A](xs.toRefArray)
}
