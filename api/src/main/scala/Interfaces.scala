package psp
package api

import Api._

trait HasSize        extends Any                           { def size: Size     }
trait HasAtomicSize  extends Any with HasSize with IsEmpty { def size: Atomic   }
trait HasPreciseSize extends Any with HasAtomicSize        { def size: Precise  }

/** Name-based extractor methods. These interfaces aren't necessary
 *  for it (thus "name-based") but provide helpful structure when used.
 */
trait IsEmpty extends Any              { def isEmpty: Boolean }
trait Opt[+A] extends Any with IsEmpty { def get: A           }

trait Each[@fspec +A]    extends Any with Foreach[A] with NotView        { def foreach(f: A => Unit): Unit   }
trait Indexed[@fspec +A] extends Any with Each[A]                        { def elemAt(i: Index): A           }
trait Direct[@fspec +A]  extends Any with Indexed[A] with HasPreciseSize
trait Linear[@fspec +A]  extends Any with Each[A]    with IsEmpty        { def head: A ; def tail: Linear[A] }

trait InMap[-K, +V] extends Any                  { def lookup: Fun[K, V]                               }
trait ExSet[A]      extends Any with Each[A]     { def apply(x: A): Bool ; def equiv(x: A, y: A): Bool }
trait ExMap[K, +V]  extends Any with InMap[K, V] { def lookup: FiniteDom[K, V]                         }

trait Index extends Any with Opt[Long]

sealed trait MaybeView extends Any                { def isView: Boolean      }
trait IsView           extends Any with MaybeView { final def isView = true  }
trait NotView          extends Any with MaybeView { final def isView = false }

trait Foreach[+A] extends Any with MaybeView with HasSize {
  def foreach(f: A => Unit): Unit
}
trait AnyView[@fspec +A] extends Any with IsView with Foreach[A] {
  type MapTo[+X] <: AnyView[X]
}

/** Contiguous operations share the property that the result is always
 *  a (possibly empty) uninterrupted subsequence of the elements of the
 *  target collection.
 */
trait ContiguousViewOps[@fspec +A] extends Any with AnyView[A] {
  type Contiguous[+X] <: View[X]

  def drop(n: Precise): Contiguous[A]
  def dropRight(n: Precise): Contiguous[A]
  def dropWhile(p: ToBool[A]): Contiguous[A]
  def take(n: Precise): Contiguous[A]
  def takeRight(n: Precise): Contiguous[A]
  def takeWhile(p: ToBool[A]): Contiguous[A]
}
trait NonContiguousViewOps[@fspec +A] extends Any with AnyView[A] {
  def collect[B](pf: A ?=> B): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def flatMap[B](f: A => Foreach[B]): MapTo[B]
  def withFilter(p: ToBool[A]): MapTo[A]
}

trait View[@fspec +A] extends Any with ContiguousViewOps[A] with NonContiguousViewOps[A] {
  type MapTo[+X] <: View[X]
  def viewOps: Direct[Doc]
}

trait InvariantView[A] extends Any with View[A] {
  def join(that: InvariantView[A]): InvariantView[A]
  def partition(p: ToBool[A]): SplitView[A]
  def span(p: ToBool[A]): SplitView[A]
  def splitAt(index: Index): SplitView[A]
}

/** When a View is split into two disjoint views.
 *  Notably, that's span, partition, and splitAt.
 */
trait SplitView[@fspec +A] extends Any with SplitM[View, A]
trait SplitInvariantView[@fspec A] extends Any with SplitInvariantM[View, A]

trait SplitM[M[+X], @fspec +A] extends Any {
  def left: M[A]   // the elements in the left-hand M.
  def right: M[A]  // the elements in the right-hand M.
  def rejoin: M[A] // Moral equivalent of left ++ right.
}
trait SplitInvariantM[M[X], @fspec A] extends Any {
  def left: M[A]   // the elements in the left-hand M.
  def right: M[A]  // the elements in the right-hand M.
  def rejoin: M[A] // Moral equivalent of left ++ right.
  def mapLeft(f: ToSelf[M[A]]): SplitInvariantM[M, A]
  def mapRight(f: ToSelf[M[A]]): SplitInvariantM[M, A]
}

/** When a View presents as a sequence of pairs.
 *  There may be two underlying views being zipped, or one view holding pairs.
 */
trait ZipView[@fspec +A1, @fspec +A2] extends Any {
  def lefts: View[A1]        // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]       // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2]  // the pairs. Moral equivalent of lefts zip rights.
}

/** When a Show type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any { def to_s: String }
trait ForceShowDirect extends Any with ShowDirect { override def toString = to_s }
