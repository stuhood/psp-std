package psp
package api

import ApiAliases._

/** A type class for decomposing an R into an A and a B.
 */
trait PairDown[-R, +A, +B] {
  def left(x: R): A
  def right(x: R): B
  def pair(x: R): A -> B
}
/** The complementary type class to PairDown.
 *  Composes an A and a B into an R.
 */
trait PairUp[+R, -A, -B] {
  def create(x: A, y: B): R
}

/** When a View is split into two disjoint views.
 *  Notably, that's span, partition, and splitAt.
 */
trait SplitView[+A] {
  def left: View[A]   // the elements in the left-hand view.
  def right: View[A]  // the elements in the right-hand view.
  def rejoin: View[A] // Moral equivalent of left ++ right.
}
/** When a View presents as a sequence of pairs.
 *  There may be two underlying views being zipped, or one view holding pairs.
 */
trait ZipView[+A1, +A2] {
  def lefts: View[A1]        // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]       // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2]  // the pairs. Moral equivalent of lefts zip rights.
}
