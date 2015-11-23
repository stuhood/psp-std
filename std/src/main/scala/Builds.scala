package psp
package std

import java.util.stream.Stream.{ builder => jStreamBuilder }

/** The organization of the implicit builders is a black art.
 *  It might be better to generate StdBuilds[0-15] and put an implicit in
 *  each one so the prioritization is as unambiguous as scala allows.
 *
 *  We adapt CanBuildFrom into our builder, since there are zillions of them lying
 *  around and it lets us build scala collections at the end of a view with no more code.
 *  Scala maps are built with Tuples even though Product2 should suffice; the types
 *  are written out as Tuple2[K, V] and not (K, V) to emphasize I'm using Tuple on purpose.
 *  The rest of the time one should write it K -> V.
 */
import api._

object Built {
  class Helper[To] {
    def apply[A](xs: Foreach[A])(implicit z: Builds[A, To]): To = z build xs
  }
  def apply[To] : Helper[To] = new Helper[To]
}

object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To] = new Impl(f)

  def array[A: CTag]: Builds[A, Array[A]]                                                    = apply(xs => doto(Array.newBuilder[A])(_ ++= xs.trav).result)
  def direct[A](): Builds[A, Vec[A]]                                                         = new Vec.Builder[A]
  def exMap[K : Eq, V] : Builds[K -> V, ExMap[K, V]]                                         = jMap[K, V] map (ExMap fromJava _)
  def exSet[A : Eq]: Builds[A, ExSet[A]]                                                     = jSet[A] map (ExSet fromJava _)
  def list[A](): Builds[A, Plist[A]]                                                         = new Plist.Builder[A]
  def sCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]                   = apply(xs => doto(z())(b => xs foreach (b += _)) result)
  def sMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = apply(xs => doto(z())(b => xs foreach (x => b += (fst(x) -> snd(x)))) result)
  def string(): Builds[Char, String]                                                         = apply(xs => doto(new StringBuilder)(b => xs foreach (c => b append c)) toString)

  def jList[A](): Builds[A, jList[A]]                             = buildsJList(new jArrayList[A])
  def jMap[K, V](): Builds[K -> V, jMap[K, V]]                    = buildsJMap(new jHashMap[K, V])
  def jSet[A](): Builds[A, jSet[A]]                               = buildsJSet(new jHashSet[A])
  def jSortedMap[K: Order, V](): Builds[K -> V, jSortedMap[K, V]] = buildsJMap(new jTreeMap[K, V](Order.comparator[K]))
  def jSortedSet[A: Order](): Builds[A, jSortedSet[A]]            = buildsJSet(new jTreeSet[A](Order.comparator[A]))
  def jStream[A](): Builds[A, jStream[A]]                         = apply(xs => doto(jStreamBuilder[A]())(xs foreach _.add).build)

  private def buildsJList[A, M[A] <: jList[A]](z: M[A]): Builds[A, M[A]]                 = apply(xs => doto(z)(z => xs foreach (x => z add x)))
  private def buildsJSet[A, M[A] <: jSet[A]](z: M[A]): Builds[A, M[A]]                   = apply(xs => doto(z)(z => xs foreach (x => z add x)))
  private def buildsJMap[K, V, M[K, V] <: jMap[K, V]](z: M[K, V]): Builds[K->V, M[K, V]] = apply(xs => doto(z)(z => xs foreach (x => z.put(fst(x), snd(x)))))

  final class Impl[Elem, To](val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Foreach[Elem]): To   = f(xs)
    def apply(mf: Suspended[Elem]): To = build(Each(mf))
  }
}
