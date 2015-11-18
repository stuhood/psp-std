package psp
package std

import api._
import scala.{ collection => sc }
import scala.math.Numeric
import psp.std.{ lowlevel => ll }

/** Implicits handling the way in and the way out of psp collections.
 */
trait StdImplicits extends scala.AnyRef
      with StdBuilds
      with StdOps
      with SetAndMapOps
      with StdUniversal {

  self =>

  implicit def typeclassTupleCleave[A, B] : Pair.Cleave[A -> B, A, B]        = Pair.Cleave[A -> B, A, B](_ -> _, fst, snd)
  implicit def typeclassLinearSplit[A] : Pair.Split[Linear[A], A, Linear[A]] = Pair.Split(_.head, _.tail)

  implicit def opsPspDirect[A](xs: Direct[A]): ops.DirectOps[A]         = new ops.DirectOps(xs)
  implicit def convertViewEach[A](xs: View[A]): Each[A]                 = Each(xs foreach _)
  implicit def opsSplitView[A](xs: SplitView[A]): Split[A]              = Split(xs.left, xs.right)

  // Promotion of the api type (which has as few methods as possible) to the
  // concrete type which has all the other ones.

  implicit def promoteApiIndex(x: Index): IndexImpl                             = Index impl x
  implicit def promoteApiOrder[A](ord: Order[A]): Order.Impl[A]                 = Order(ord.compare)
  implicit def promoteApiExSet[A](x: ExSet[A]): ExSet.Impl[A]                   = ExSet impl x
  implicit def promoteApiExMap[K, V](x: ExMap[K, V]): ExMap.Impl[K, V]          = ExMap impl x
  implicit def promoteApiView[A](xs: View[A]): BaseView[A, _]                   = xs match { case xs: BaseView[A, _] => xs }
  implicit def promoteInvariantApiView[A](xs: InvariantView[A]): BaseView[A, _] = xs match { case xs: BaseView[A, _] => xs }
  implicit def promoteApiInSet[A](x: InSet[A]): InSet.Impl[A]                   = InSet impl x
  implicit def promoteApiInMap[K, V](x: InMap[K, V]): InMap.Impl[K, V]          = InMap impl x
}

trait GlobalShow0 {
  implicit def convertHasClassTagTryDoc[A](x: A)(implicit z: CTag[A]): TryDoc = TryDoc.NoDoc(x, z)
}
trait GlobalShow extends GlobalShow0 {
  implicit def convertHasShowDocOps[A: Show](x: A): DocOps          = new DocOps(Doc(x))
  implicit def convertHasShowDoc[A](x: A)(implicit z: Show[A]): Doc = Doc(x)
}

trait SetAndMapOps {
  implicit def opsExtensionalSet[A](x: ExSet[A]): ops.ExSetOps[A]          = new ops.ExSetOps(x)
  implicit def opsExtensionalMap[K, V](x: ExMap[K, V]): ops.ExMapOps[K, V] = new ops.ExMapOps(x)
}

trait StdOps0 {
  implicit def opsPspUnbuilt[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Unbuilder[A, R] = new Unbuilder(xs)
  implicit def opsForeach[A](xs: Each[A]): ops.ForeachOps[A]                             = new ops.ForeachOps(xs)
}
trait StdOps1 extends StdOps0 {
  implicit def convertViewBuilds[A, CC[A]](xs: View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build xs
  implicit def opsHasEq[A: Eq](x: View[A]): ops.HasEq[A]       = new ops.HasEq(x)
}
trait StdOps2 extends StdOps1 {
  implicit def opsDirectArray[A](xs: Array[A]): ops.DirectOps[A] = new ops.DirectOps(Direct fromArray xs)
  implicit def opsDirectString(s: String): ops.DirectOps[Char]   = new ops.DirectOps(Direct fromString s)

  // We buried Predef's {un,}augmentString in favor of these.
  implicit def pspAugmentString(x: String): PspStringOps               = new PspStringOps(x)
  implicit def opsAtomicView[A](x: View[A]): ops.InvariantViewOps[A]   = new ops.InvariantViewOps(x)
  implicit def opsHasOrderInfix[A: Order](x: A): ops.infix.OrderOps[A] = new ops.infix.OrderOps[A](x)
  implicit def opsHasHash[A: Hash](x: View[A]): ops.HasHash[A]         = new ops.HasHash(x)
}

trait StdOps3 extends StdOps2 {
  // We're (sickly) using the context bound to reduce the applicability of the implicit,
  // but then discarding it. The only way these can be value classes is if the type class
  // arrives with the method call.
  implicit def opsHasAlgebraInfix[A: BooleanAlgebra](x: A): ops.infix.AlgebraOps[A] = new ops.infix.AlgebraOps[A](x)
  implicit def opsHasEqInfix[A: Eq](x: A): ops.infix.EqOps[A]                       = new ops.infix.EqOps[A](x)
  implicit def opsHasHashInfix[A: Hash](x: A): ops.infix.HashOps[A]                 = new ops.infix.HashOps[A](x)

  implicit def opsHasOrder[A: Order](x: View[A]): ops.HasOrder[A] = new ops.HasOrder(x)
  implicit def opsHasEmpty[A: Empty](x: View[A]): ops.HasEmpty[A] = new ops.HasEmpty[A](x)

  implicit def opsHasShowEach[A: Show](x: Each[A]): ops.DocSeqOps = new ops.DocSeqOps(x map (_.doc) toVec)
  implicit def opsHasShowView[A: Show](x: View[A]): ops.DocSeqOps = opsHasShowEach(x)

  implicit def opsBoolean(x: Boolean): ops.BooleanOps                               = new ops.BooleanOps(x)
  implicit def opsBooleanAlgebra[A](x: BooleanAlgebra[A]): ops.BooleanAlgebraOps[A] = new ops.BooleanAlgebraOps[A](x)
  implicit def opsChar(x: Char): ops.CharOps                                        = new ops.CharOps(x)
  implicit def opsFileTime(x: jFileTime): ops.FileTimeOps                           = new ops.FileTimeOps(x)
  implicit def opsHasPreciseSize(x: HasPreciseSize): ops.HasPreciseSizeOps          = new ops.HasPreciseSizeOps(x)
  implicit def opsJavaIterator[A](x: jIterator[A]): ops.JavaIteratorOps[A]          = new ops.JavaIteratorOps[A](x)
  implicit def opsInputStream(x: InputStream): ops.InputStreamOps                   = new ops.InputStreamOps(x)
  implicit def opsInt(x: Int): ops.IntOps                                           = new ops.IntOps(x)
  implicit def opsLong(x: Long): ops.LongOps                                        = new ops.LongOps(x)
  implicit def opsOption[A](x: Option[A]): ops.OptionOps[A]                         = new ops.OptionOps[A](x)
  implicit def opsPartialFunction[A, B](pf: A ?=> B): ops.PartialFunctionOps[A, B]  = new ops.PartialFunctionOps(pf)
  implicit def opsPrecise(x: Precise): ops.PreciseOps                               = new ops.PreciseOps(x)
  implicit def opsSize(x: Size): ops.SizeOps                                        = new ops.SizeOps(x)
  implicit def opsStdOpt[A](x: Opt[A]): ops.StdOptOps[A]                            = new ops.StdOptOps[A](x)
  implicit def opsTry[A](x: Try[A]): ops.TryOps[A]                                  = new ops.TryOps[A](x)
  implicit def opsZipView[A, B](xs: ZipView[A, B]): ops.ZipViewOps[A, B]            = new ops.ZipViewOps(xs)
  implicit def opsUnit(x: Unit): ops.UnitOps.type                                   = ops.UnitOps
}

trait StdOps extends StdOps3 {
  implicit def opsStringContext(sc: StringContext): ShowInterpolator                      = new ShowInterpolator(sc)
  implicit def convertPredicateStreamFilter[A](p: ToBool[A]): DirectoryStreamFilter[A]    = new DirectoryStreamFilter[A] { def accept(entry: A) = p(entry) }
  implicit def convertPredicatePartialFunction[A](p: ToBool[A]): A ?=> A                  = { case x if p(x) => x }
  implicit def opsViewConversions[A](xs: View[A]): Conversions[A]                         = new Conversions(Each[A](xs foreach _))
  implicit def unbuildableConv[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Conversions[A] = new Conversions[A](z unbuild xs)
}

trait StdUniversal0 {
  implicit def opsAny[A](x: A): ops.AnyOps[A] = new ops.AnyOps[A](x)
  // Lower priority than the hand-specialized variations.
  final implicit def arrowAssocRef[A](x: A): ll.ArrowAssocRef[A] = new ll.ArrowAssocRef(x)
}
trait StdUniversal extends StdUniversal0 {
  // Prefer opsAnyRef over opsAny.
  implicit def opsAnyRef[A <: AnyRef](x: A): ops.AnyRefOps[A] = new ops.AnyRefOps[A](x)

  final implicit def arrowAssocInt(x: Int): ll.ArrowAssocInt             = new ll.ArrowAssocInt(x)
  final implicit def arrowAssocLong(x: Long): ll.ArrowAssocLong          = new ll.ArrowAssocLong(x)
  final implicit def arrowAssocDouble(x: Double): ll.ArrowAssocDouble    = new ll.ArrowAssocDouble(x)
  final implicit def arrowAssocChar(x: Char): ll.ArrowAssocChar          = new ll.ArrowAssocChar(x)
  final implicit def arrowAssocBoolean(x: Boolean): ll.ArrowAssocBoolean = new ll.ArrowAssocBoolean(x)
}

/*** The builder/unbuilder/view hierarchy.
 */

trait JavaBuilds0 {
  implicit def buildJavaSet[A]: Builds[A, jSet[A]]                                                   = Builds.jSet[A]
  implicit def buildJavaList[A]: Builds[A, jList[A]]                                                 = Builds.jList[A]
  implicit def buildJavaMap[K, V]: Builds[K -> V, jMap[K, V]]                                        = Builds.jMap[K, V]
  implicit def viewJavaIterable[A, CC[X] <: jIterable[X]](xs: CC[A]): AtomicView[A, CC[A]]           = View(xs)
  implicit def viewJavaMap[K, V, CC[K, V] <: jMap[K, V]](xs: CC[K, V]): AtomicView[K -> V, CC[K, V]] = View(xs)
  implicit def viewDirectoryStream[A](stream: DirectoryStream[A]): View[A]                           = inView(BiIterable(stream) foreach _)

  implicit def unbuildJavaIterable[A, CC[X] <: jIterable[X]] : UnbuildsAs[A, CC[A]]        = Unbuilds[A, CC[A]](Each fromJava _)
  implicit def unbuildJavaMap[K, V, CC[K, V] <: jMap[K, V]] : UnbuildsAs[K -> V, CC[K, V]] = Unbuilds[K -> V, CC[K, V]](Each fromJavaMap _)
  implicit def unbuiltPspView0[A] : UnbuildsAs[A, View[A]]                                 = Unbuilds[A, View[A]](xs => xs)
}
trait JavaBuilds extends JavaBuilds0 {
  implicit def buildJavaSortedSet[A: Order]: Builds[A, jSortedSet[A]] = Builds.jSortedSet[A]

  implicit def unbuiltPspView1[A, R] : UnbuildsAs[A, BaseView[A, R]] = Unbuilds[A, BaseView[A, R]](xs => xs)
}

trait ScalaBuilds extends JavaBuilds {
  implicit def unbuildScalaCollection[A, CC[X] <: GTOnce[X]] : UnbuildsAs[A, CC[A]]             = Unbuilds[A, CC[A]](Each fromScala _)
  implicit def buildScalaCollection[A, That](implicit z: CanBuild[A, That]): Builds[A, That]    = Builds.sCollection[A, That]
  implicit def viewScalaCollection[A, CC[X] <: sCollection[X]](xs: CC[A]): AtomicView[A, CC[A]] = View fromScala xs

  implicit def unbuildScalaMap[K, V, CC[X, Y] <: scMap[X, Y]] : UnbuildsAs[K -> V, CC[K, V]]                   = Unbuilds[K -> V, CC[K, V]](Each fromScala _)
  implicit def buildScalaMap[K, V, That](implicit z: CanBuild[scala.Tuple2[K, V], That]): Builds[K -> V, That] = Builds.sMap[K, V, That]
  implicit def viewScalaIndexedSeq[A, CC[X] <: sciIndexedSeq[X]](xs: CC[A]): DirectView[A, CC[A]]              = View direct (Direct fromScala xs)

  implicit def buildPspSet[A: Eq]: Builds[A, ExSet[A]]                           = Builds.exSet[A]
  implicit def buildPspMap[K: Eq, V]: Builds[K -> V, ExMap[K, V]]                = Builds.exMap[K, V]
}
trait StdBuilds0 extends ScalaBuilds {
  implicit def buildPspLinear[A] : Builds[A, Plist[A]]                           = Plist.newBuilder[A]
  implicit def viewPspEach[A, CC[X] <: Each[X]](xs: CC[A]): AtomicView[A, CC[A]] = View each xs
  implicit def unbuildPspEach[A, CC[X] <: Each[X]] : UnbuildsAs[A, CC[A]]        = Unbuilds[A, CC[A]](xs => xs)
}
trait StdBuilds1 extends StdBuilds0 {
  implicit def unbuiltPspArray[A] : UnbuildsAs[A, Array[A]]           = Unbuilds[A, Array[A]](Direct fromArray _)
  implicit def buildPspArray[A: CTag]: Builds[A, Array[A]]            = Builds.array[A]
  implicit def viewPspArray[A](xs: Array[A]): DirectView[A, Array[A]] = View direct (Direct fromArray xs)
}
trait StdBuilds2 extends StdBuilds1 {
  implicit def buildPspDirect[A] : Builds[A, Vec[A]]                                 = Vec.newBuilder[A] // Builds.direct[A]
  implicit def viewPspDirect[A, CC[X] <: Direct[X]](xs: CC[A]): DirectView[A, CC[A]] = View direct xs
}
trait StdBuilds extends StdBuilds2 {
  implicit def unbuildPspString: UnbuildsAs[Char, String]          = Unbuilds[Char, String](Direct fromString _)
  implicit def buildPspString: Builds[Char, String]                = Builds.string
  implicit def viewPspString(xs: String): DirectView[Char, String] = View direct (Direct fromString xs)
}
