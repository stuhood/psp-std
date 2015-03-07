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
object ImmutableArray {
  def apply[@spec A : CTag](xs: A*): ImmutableArray[A] = new ImmutableArray[A](xs.toArray)
}
