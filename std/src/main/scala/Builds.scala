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
import com.google.common.collect._

object guavabuilder {
  def mapBuilder[K, V](): ImmutableMap.Builder[K, V]                    = ImmutableMap.builder[K, V]
  def setBuilder[A](): ImmutableSet.Builder[A]                          = ImmutableSet.builder[A]
  def sortedMapBuilder[K: Order, V](): ImmutableSortedMap.Builder[K, V] = ImmutableSortedMap orderedBy ?[Order[K]].toComparator
  def sortedSetBuilder[A: Order](): ImmutableSortedSet.Builder[A]       = ImmutableSortedSet orderedBy ?[Order[A]].toComparator
}

trait StdBuilds0 {
  implicit def buildsLinear[A] : Builds[A, Linear[A]]                                         = Builds.linear[A]
  implicit def buildsJavaList[A]: Builds[A, jList[A]]                                         = Builds.jList[A]
  implicit def buildsJavaSet[A]: Builds[A, jSet[A]]                                           = Builds.jSet[A]
  implicit def buildsJavaMap[K, V]: Builds[K -> V, jMap[K, V]]                                = Builds.jMap[K, V]
  implicit def buildsScalaCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That] = Builds.sCollection[A, That]
  implicit def buildsArray[A: CTag]: Builds[A, Array[A]]                                      = Builds.array[A]
}
trait StdBuilds1 extends StdBuilds0 {
  implicit def buildsScalaMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = Builds.sMap[K, V, That]
  implicit def buildsExSet[A: Eq]: Builds[A, ExSet[A]]                                                          = Builds.exSet[A]
  implicit def buildsExMap[K: Eq, V]: Builds[K -> V, ExMap[K, V]]                                               = Builds.exMap[K, V]
}
trait StdBuilds2 extends StdBuilds1 { implicit def buildsDirect[A] : Builds[A, Direct[A]] = Builds.direct[A] }
trait StdBuilds extends StdBuilds2  { implicit def buildsString: Builds[Char, String]     = Builds.string    }

trait StdUnbuilds {
  implicit def unbuildsEach[A, CC[X] <: Each[X]] : Unbuilds[A, CC[A]]                       = Unbuilds[A, CC[A]](xs => xs)
  implicit def unbuildsScalaCollection[A, CC[X] <: sCollection[X]] : Unbuilds[A, CC[A]]     = Unbuilds[A, CC[A]](Each fromScala _)
  implicit def unbuildsJavaCollection[A, CC[X] <: jIterable[X]] : Unbuilds[A, CC[A]]        = Unbuilds[A, CC[A]](Each fromJava _)
  implicit def unbuildsJavaMap[K, V, CC[K, V] <: jMap[K, V]] : Unbuilds[K -> V, CC[K, V]]   = Unbuilds[K -> V, CC[K, V]](Each fromJavaMap _)
  implicit def unbuildsArray[A] : Unbuilds[A, Array[A]]                                     = Unbuilds[A, Array[A]](Direct fromArray _)
  implicit def unbuildsString: Unbuilds[Char, String]                                       = Unbuilds[Char, String](Direct fromString _)
  implicit def unbuildsScalaMap[K, V, CC[X, Y] <: scMap[X, Y]] : Unbuilds[K -> V, CC[K, V]] = Unbuilds[K -> V, CC[K, V]](Each fromScala _)
}

object Builds {
  def apply[Elem, To](f: Each[Elem] => To): Builds[Elem, To] = new Impl(f)

  def array[A: CTag]: Builds[A, Array[A]]                                                    = array(Array.newBuilder[A])
  def direct[A](): Builds[A, Direct[A]]                                                      = direct(sciVector.newBuilder[A])
  def each[A]: Builds[A, Each[A]]                                                            = apply(identity)
  def exMap[K : Eq, V] : Builds[K -> V, ExMap[K, V]]                                         = gSortedMap[K, V] map (ExMap fromJava _)
  def exSet[A : Eq]: Builds[A, ExSet[A]]                                                     = gSortedSet[A] map (ExSet fromJava _)
  def jList[A](): Builds[A, jList[A]]                                                        = jList(new jArrayList[A])
  def jMap[K, V](): Builds[K -> V, jMap[K, V]]                                               = jMap(new jHashMap[K, V])
  def jSet[A](): Builds[A, jSet[A]]                                                          = jSet(new jHashSet[A])
  def linear[A](): Builds[A, Linear[A]]                                                      = linear(sciList.newBuilder[A])
  def sCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]                   = apply(xs => z() doto (b => xs foreach (b += _)) result)
  def sMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = apply(xs => z() doto (b => xs foreach (x => b += (fst(x) -> snd(x)))) result)
  def string(): Builds[Char, String]                                                         = apply(xs => new StringBuilder doto (b => xs foreach (c => b append c)) result)

  def gSortedSet[A: Eq]: Builds[A, gSortedSet[A]]            = apply(gSetBuilder[A]() addAll _.iterator build)
  def gSortedMap[K: Eq, V]: Builds[K -> V, gSortedMap[K, V]] = apply(xs => gMapBuilder[K, V]() doto (b => xs foreach (x => b.put(x._1, x._2))) build)

  private def gMapBuilder[K: Eq, V](): ImmutableSortedMap.Builder[K, V]       = ImmutableSortedMap orderedBy Eq.eqComparator[K]()
  private def gSetBuilder[A: Eq](): ImmutableSortedSet.Builder[A]             = ImmutableSortedSet orderedBy Eq.eqComparator[A]()
  private def array[A](b: scmBuilder[A, Array[A]]): Builds[A, Array[A]]       = apply(b ++= _.trav result)
  private def direct[A](b: scmBuilder[A, sciVector[A]]): Builds[A, Direct[A]] = apply[A, sciVector[A]](xs => b doto (xs foreach _.+=) result) map Direct.fromScala
  private def linear[A](b: scmBuilder[A, sciList[A]]): Builds[A, Linear[A]]   = apply[A, sciList[A]](xs => b doto (xs foreach _.+=) result) map Linear.fromScala
  private def jList[A](js: jArrayList[A]): Builds[A, jList[A]]                = apply(xs => js doto (js => xs foreach (js add _)))
  private def jMap[K, V](js: jHashMap[K, V]): Builds[K -> V, jMap[K, V]]      = apply(xs => js doto (js => xs foreach (kv => js.put(fst(kv), snd(kv)))))
  private def jSet[A](js: jHashSet[A]): Builds[A, jSet[A]]                    = apply(xs => js doto (js => xs foreach js.add))

  final class Impl[Elem, To](val f: Each[Elem] => To) extends AnyVal with Builds[Elem, To] {
    def build(xs: Each[Elem]): To      = f(xs)
    def apply(mf: Suspended[Elem]): To = build(Each(mf))
  }
}
object Unbuilds {
  def apply[A, Repr](f: Repr => Each[A]): Unbuilds[A, Repr] = new Impl(f)

  final class Impl[A, Repr](val f: Repr => Each[A]) extends AnyVal with Unbuilds[A, Repr] {
    def unbuild(xs: Repr): Each[A] = f(xs)
  }
  final class Create[A, Repr](repr: Repr)(implicit z: Unbuilds[A, Repr]) extends ops.ByOps[A] {
    def xs = m
    def m: AtomicView[A, Repr] = z unbuild repr match {
      case xs: Direct[A] => new DirectView[A, Repr](xs)
      case xs            => new LinearView[A, Repr](xs)
    }
  }
}
