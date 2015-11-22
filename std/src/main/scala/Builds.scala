package psp
package std

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

object Builds {
  def apply[Elem, To](f: Foreach[Elem] => To): Builds[Elem, To] = new Impl(f)

  def array[A: CTag]: Builds[A, Array[A]]                                                    = array(Array.newBuilder[A])
  def direct[A](): Builds[A, Vec[A]]                                                         = Vec.newBuilder[A]
  def list[A](): Builds[A, Plist[A]]                                                         = Plist.newBuilder[A]
  def exMap[K : Eq, V] : Builds[K -> V, ExMap[K, V]]                                         = jMap[K, V] map (ExMap fromJava _)
  def exSet[A : Eq]: Builds[A, ExSet[A]]                                                     = jSet[A] map (ExSet fromJava _)
  def jList[A](): Builds[A, jList[A]]                                                        = jList(new jArrayList[A])
  def jMap[K, V](): Builds[K -> V, jMap[K, V]]                                               = jMap(new jHashMap[K, V])
  def jSet[A](): Builds[A, jSet[A]]                                                          = jSet(new jHashSet[A])
  def jSortedSet[A: Order](): Builds[A, jSortedSet[A]]                                       = jSortedSet(new jTreeSet[A](Order.comparator[A]))
  def jSortedMap[K: Order, V](): Builds[K -> V, jSortedMap[K, V]]                            = jSortedMap(new jTreeMap[K, V](Order.comparator[K]))
  def sCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]                   = apply(xs => doto(z())(b => xs foreach (b += _)) result)
  def sMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = apply(xs => doto(z())(b => xs foreach (x => b += (fst(x) -> snd(x)))) result)
  def string(): Builds[Char, String]                                                         = apply(xs => doto(new StringBuilder)(b => xs foreach (c => b append c)) toString)

  private def array[A](b: scmBuilder[A, Array[A]]): Builds[A, Array[A]]  = apply(b ++= _.trav result)
  private def jList[A](js: jArrayList[A]): Builds[A, jList[A]]           = apply(xs => doto(js)(js => xs foreach (js add _)))
  private def jMap[K, V](js: jHashMap[K, V]): Builds[K -> V, jMap[K, V]] = apply(xs => doto(js)(js => xs foreach (kv => js.put(fst(kv), snd(kv)))))
  private def jSet[A](js: jHashSet[A]): Builds[A, jSet[A]]               = apply(xs => doto(js)(js => xs foreach js.add))
  private def jSortedSet[A](js: jTreeSet[A]): Builds[A, jSortedSet[A]]   = apply(xs => doto(js)(js => xs foreach js.add))

  private def jSortedMap[K, V](js: jTreeMap[K, V]): Builds[K -> V, jSortedMap[K, V]] =
    apply(xs => doto(js)(js => xs foreach (kv => js.put(fst(kv), snd(kv)))))

  final class Impl[Elem, To](val f: Foreach[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Foreach[Elem]): To   = f(xs)
    def apply(mf: Suspended[Elem]): To = build(Each(mf))
  }
}
