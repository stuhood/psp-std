package psp
package std
package ops

import api._

/** If we user the initial value as a jumping-off point can we improve the
 *  interface to folds?
 */
trait FoldOps[+A, B] extends Any {
  def xs: View[A]
  def initial: B

  def indexed(f: (B, A, Index) => B): B = {
    var res = initial
    xs.zipIndex foreach ((x, i) => res = f(res, x, i))
    res
  }
  def left(f: (B, A) => B): B = {
    var res = initial
    xs foreach (x => res = f(res, x))
    res
  }
  def right(f: (A, B) => B): B = {
    var res = initial
    xs foreachReverse (x => res = f(x, res))
    res
  }
}
final case class FoldOpsClass[+A, B](xs: View[A], initial: B) extends FoldOps[A, B]
