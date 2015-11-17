package psp
package api

/** API level type classes and interfaces for views and collections.
 *
 *  An intensional collection is one which can tell us things about
 *  instances of a particular type, but cannot produce any of that type.
 *  An extensional collection is the dual: it can produce instances of
 *  a particular type, but can tell us nothing about them.
 *
 *  Alert readers will notice we are describing contravariance and
 *  covariance, and indeed those properties accompany those traits.
 *
 *  An intensional set (InSet) is little more than a function A => Bool
 *  plus some structure. An ExSet is little more than a generator of As.
 */
import Api._

trait HasSize        extends Any                           { def size: Size     }
trait HasAtomicSize  extends Any with HasSize with IsEmpty { def size: Atomic   }
trait HasPreciseSize extends Any with HasAtomicSize        { def size: Precise  }

/** Name-based extractor methods. These interfaces aren't necessary
 *  for it (thus "name-based") but provide helpful structure when used.
 */
trait IsEmpty extends Any              { def isEmpty: Boolean }
trait Opt[+A] extends Any with IsEmpty { def get: A           }

trait Each[@spec(SpecTypes) +A]      extends Any with HasSize                        { def foreach(f: A => Unit): Unit    }
trait Indexed[@spec(SpecTypes) +A]   extends Any with Each[A]                        { def elemAt(i: Index): A            }
trait Direct[@spec(SpecTypes) +A]    extends Any with Indexed[A] with HasPreciseSize
trait Linear[@spec(SpecTypes) +A]    extends Any with Each[A]    with IsEmpty        { def head: A ; def tail: Linear[A]  }

trait InSet[-A]     extends Any                            { def apply(x: A): Boolean       }
trait InMap[-K, +V] extends Any                            { def lookup: Fun[K, V]          }
trait ExSet[A]      extends Any with Each[A] with InSet[A] { def equiv(x: A, y: A): Boolean }
trait ExMap[K, +V]  extends Any with InMap[K, V]           { def lookup: FiniteDom[K, V]    }

// TODO - maybe.
// final case class LongIndex(x: Long) extends AnyVal with Index { def isEmpty = x < 0 ; def get: Long = x }
// final case class IntIndex(x: Int) extends AnyVal with Index   { def isEmpty = x < 0 ; def get: Long = x }
trait Index extends Any with Opt[Long]

trait AnyView[@spec(SpecTypes) +A] extends Any with HasSize {
  type MapTo[+X] <: AnyView[X]

  def foreach(f: A => Unit): Unit
}

/** Contiguous operations share the property that the result is always
 *  a (possibly empty) uninterrupted subsequence of the elements of the
 *  target collection.
 */
trait ContiguousViewOps[@spec(SpecTypes) +A] extends Any with AnyView[A] {
  type Contiguous[+X] <: View[X]

  // TODO:
  //
  // def init: Contiguous[A]
  // def tail: Contiguous[A]
  // def span(p: ToBool[A]): ContiguousSplit[A]
  // def splitAt(index: Index): ContiguousSplit[A]
  def drop(n: Precise): Contiguous[A]
  def dropRight(n: Precise): Contiguous[A]
  def dropWhile(p: ToBool[A]): Contiguous[A]
  def take(n: Precise): Contiguous[A]
  def takeRight(n: Precise): Contiguous[A]
  def takeWhile(p: ToBool[A]): Contiguous[A]
}

trait NonContiguousViewOps[@spec(SpecTypes) +A] extends Any with AnyView[A] {
  def collect[B](pf: A ?=> B): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def flatMap[B](f: A => Each[B]): MapTo[B]
  def withFilter(p: ToBool[A]): MapTo[A]
}

trait View[@spec(SpecTypes) +A] extends Any with ContiguousViewOps[A] with NonContiguousViewOps[A] {
  type MapTo[+X] <: View[X]
  def viewOps: Direct[Doc]
}

trait ContiguousView[@spec(SpecTypes) +A] extends Any with View[A] {
  type Contiguous[+X] <: ContiguousView[X]
}

trait InvariantView[A] extends Any with View[A] {
  def join(that: InvariantView[A]): InvariantView[A]
  def partition(p: ToBool[A]): SplitView[A]
  def span(p: ToBool[A]): SplitView[A]
  def splitAt(index: Index): SplitView[A]
}

trait InMapView[-K, +V] extends Any with AnyView[V] with InMap[K, V] {
  type CoMapTo[-X] <: InMapView[X, V]
  type MapTo[+X] <: InMapView[K, X]

  def comap[K1](f: K1 => K): CoMapTo[K1]
  def filter(p: ToBool[V]): MapTo[V]
  def map[V1](f: V => V1): MapTo[V1]
}

/** When a View is split into two disjoint views.
 *  Notably, that's span, partition, and splitAt.
 */
trait SplitView[@spec(SpecTypes) +A] extends Any with SplitM[View, A]
trait SplitInvariantView[@spec(SpecTypes) A] extends Any with SplitInvariantM[View, A]

trait SplitM[M[+X], @spec(SpecTypes) +A] extends Any {
  def left: M[A]   // the elements in the left-hand M.
  def right: M[A]  // the elements in the right-hand M.
  def rejoin: M[A] // Moral equivalent of left ++ right.
}
trait SplitInvariantM[M[X], @spec(SpecTypes) A] extends Any {
  def left: M[A]   // the elements in the left-hand M.
  def right: M[A]  // the elements in the right-hand M.
  def rejoin: M[A] // Moral equivalent of left ++ right.
  def mapLeft(f: ToSelf[M[A]]): SplitInvariantM[M, A]
  def mapRight(f: ToSelf[M[A]]): SplitInvariantM[M, A]
}

/** When a View presents as a sequence of pairs.
 *  There may be two underlying views being zipped, or one view holding pairs.
 */
trait ZipView[@spec(SpecTypes) +A1, @spec(SpecTypes) +A2] {
  def lefts: View[A1]        // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]       // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2]  // the pairs. Moral equivalent of lefts zip rights.
}
