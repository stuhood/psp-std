package psp
package api

/** API level type classes and interfaces for views and collections.
 */
import Api._

trait HasSize        extends Any                           { def size: Size     }
trait HasAtomicSize  extends Any with HasSize with IsEmpty { def size: Atomic   }
trait HasPreciseSize extends Any with HasAtomicSize        { def size: Precise  }
trait HasIntSize     extends Any with HasPreciseSize       { def size: IntSize  }

trait Each[+A] extends Any with HasSize                          { def foreach(f: A => Unit): Unit   }
trait Indexed[+A] extends Any with Each[A]                       { def elemAt(i: Index): A           }
trait Direct[+A] extends Any with Indexed[A] with HasPreciseSize
trait Linear[+A] extends Any with Each[A] with IsEmpty           { def head: A ; def tail: Linear[A] }

trait Intensional[-K, +V] extends Any                              { def apply(x: K): V       }
trait InSet[-A]           extends Any with Intensional[A, Boolean] { def apply(x: A): Boolean }
trait InMap[-K, +V]       extends Any with Intensional[K, V]       { def domain: InSet[K]     }

trait Extensional[+A]     extends Any with Each[A]
trait ExSet[A]            extends Any with Extensional[A] with InSet[A]         { def hashEq: HashEq[A] }
trait ExMap[K, +V]        extends Any with Extensional[K -> V] with InMap[K, V] { def domain: ExSet[K]  }

trait AnyView[+A] extends Any with HasSize {
  type MapTo[+X] <: AnyView[X]

  def foreach(f: A => Unit): Unit
  def toEach: Each[A]
}

trait SetView[A] extends Any with AnyView[A] with ExSet[A] {
}

/** Contiguous operations share the property that the result is always
 *  a (possibly empty) uninterrupted subsequence of the elements of the
 *  target collection.
 */
trait ContiguousViewOps[+A] extends Any with AnyView[A] {
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

trait NonContiguousViewOps[+A] extends Any with AnyView[A] {
  def ++[A1 >: A](that: View[A1]): View[A1]
  def collect[B](pf: A ?=> B): MapTo[B]
  def map[B](f: A => B): MapTo[B]
  def flatMap[B](f: A => Each[B]): MapTo[B]
  def withFilter(p: ToBool[A]): MapTo[A]
}

trait View[+A] extends Any with ContiguousViewOps[A] with NonContiguousViewOps[A] {
  type MapTo[+X] <: View[X]
  def viewOps: Direct[String]
}

trait ContiguousView[+A] extends Any with View[A] {
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
trait SplitView[+A] {
  def left: View[A]   // the elements in the left-hand view.
  def right: View[A]  // the elements in the right-hand view.
  def rejoin: View[A] // Moral equivalent of left ++ right.
}
trait SplitInvariantView[A] extends SplitView[A] {
  def mapLeft(f: View[A] => View[A]): SplitView[A]
  def mapRight(f: View[A] => View[A]): SplitView[A]
}
/** When a View presents as a sequence of pairs.
 *  There may be two underlying views being zipped, or one view holding pairs.
 */
trait ZipView[+A1, +A2] {
  def lefts: View[A1]        // the left element of each pair. Moral equivalent of pairs map fst.
  def rights: View[A2]       // the right element of each pair. Moral equivalent of pairs map snd.
  def pairs: View[A1 -> A2]  // the pairs. Moral equivalent of lefts zip rights.
}
