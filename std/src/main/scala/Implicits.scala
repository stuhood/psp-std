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

  implicit def convertViewEach[A](xs: View[A]): Each[A]         = Each(xs foreach _)
  implicit def opsSplitView[A](xs: SplitView[A]): Split[A]      = Split(xs.left, xs.right)

  // Promotion of the api type (which has as few methods as possible) to the
  // concrete type which has all the other ones.

  implicit def promoteApiIndex(x: Index): IndexImpl                          = Index impl x
  implicit def promoteApiOrder[A](z: Order[A]): Order.Impl[A]                = Order impl z
  implicit def promoteApiExSet[A](x: ExSet[A]): ExSet.Impl[A]                = ExSet impl x
  implicit def promoteApiExMap[K, V](x: ExMap[K, V]): ExMap.Impl[K, V]       = ExMap impl x
  implicit def promoteApiView[A](xs: View[A]): AtomicView[A, View[A]]        = View impl xs
  implicit def promoteApiInSet[A](x: InSet[A]): InSet.Impl[A]                = InSet impl x
  implicit def promoteApiInMap[K, V](x: InMap[K, V]): InMap.Impl[K, V]       = InMap impl x
  implicit def promoteApiZipView[A, B](xs: ZipView[A, B]): ZipViewImpl[A, B] = new ZipViewImpl(xs)
}

trait GlobalShow {
  implicit def convertHasShowDocOps[A: Show](x: A): DocOps          = new DocOps(Doc(x))
  implicit def convertHasShowDoc[A](x: A)(implicit z: Show[A]): Doc = Doc(x)
}

trait SetAndMapOps {
  implicit def opsExtensionalSet[A](x: ExSet[A]): ops.ExSetOps[A]          = new ops.ExSetOps(x)
  implicit def opsExtensionalMap[K, V](x: ExMap[K, V]): ops.ExMapOps[K, V] = new ops.ExMapOps(x)
}

trait StdOps0 {
  implicit def opsPspUnbuilt[A, R](xs: R)(implicit z: UnbuildsAs[A, R]): Unbuilder[A, R] = new Unbuilder(xs)
}
trait StdOps1 extends StdOps0 {
  implicit def convertViewBuilds[A, CC[A]](xs: View[A])(implicit z: Builds[A, CC[A]]): CC[A] = z build xs
  implicit def opsHasEq[A: Eq](x: View[A]): ops.HasEq[A]       = new ops.HasEq(x)
}
trait StdOps2 extends StdOps1 {
  // We buried Predef's {un,}augmentString in favor of these.
  implicit def opsWrapString(x: String): Pstring                       = new Pstring(x)
  implicit def opsAtomicView[A](x: View[A]): ops.InvariantViewOps[A]   = new ops.InvariantViewOps(x)
  implicit def opsHasOrderInfix[A: Order](x: A): ops.infix.OrderOps[A] = new ops.infix.OrderOps[A](x)
  implicit def opsHasHash[A: Hash](x: View[A]): ops.HasHash[A]         = new ops.HasHash(x)
}

trait StdOps3 extends StdOps2 {
  // We're (sickly) using the context bound to reduce the applicability of the implicit,
  // but then discarding it. The only way these can be value classes is if the type class
  // arrives with the method call.

  implicit def opsChar(x: Char): ops.CharOps                                        = new ops.CharOps(x)
  implicit def opsHasAlgebraInfix[A: BooleanAlgebra](x: A): ops.infix.AlgebraOps[A] = new ops.infix.AlgebraOps[A](x)
  implicit def opsHasEmpty[A: Empty](x: View[A]): ops.HasEmpty[A]                   = new ops.HasEmpty[A](x)
  implicit def opsHasEqInfix[A: Eq](x: A): ops.infix.EqOps[A]                       = new ops.infix.EqOps[A](x)
  implicit def opsHasHashInfix[A: Hash](x: A): ops.infix.HashOps[A]                 = new ops.infix.HashOps[A](x)
  implicit def opsHasOrder[A: Order](x: View[A]): ops.HasOrder[A]                   = new ops.HasOrder(x)
  implicit def opsHasShowEach[A: Show](x: Each[A]): ops.DocSeqOps                   = new ops.DocSeqOps(x map (_.doc) toVec)
  implicit def opsHasShowView[A: Show](x: View[A]): ops.DocSeqOps                   = opsHasShowEach(x)
  implicit def opsInputStream(x: InputStream): ops.InputStreamOps                   = new ops.InputStreamOps(x)
  implicit def opsInt(x: Int): ops.IntOps                                           = new ops.IntOps(x)
  implicit def opsJavaIterator[A](x: jIterator[A]): ops.JavaIteratorOps[A]          = new ops.JavaIteratorOps[A](x)
  implicit def opsLong(x: Long): ops.LongOps                                        = new ops.LongOps(x)
  implicit def opsOption[A](x: Option[A]): ops.OptionOps[A]                         = new ops.OptionOps[A](x)
  implicit def opsPrecise(x: Precise): ops.PreciseOps                               = new ops.PreciseOps(x)
  implicit def opsSize(x: Size): ops.SizeOps                                        = new ops.SizeOps(x)
  implicit def opsTry[A](x: Try[A]): ops.TryOps[A]                                  = new ops.TryOps[A](x)
}

trait StdOps extends StdOps3 {
  implicit def opsStringContext(sc: StringContext): ShowInterpolator                      = new ShowInterpolator(sc)
  implicit def convertPredicateStreamFilter[A](p: ToBool[A]): DirectoryStreamFilter[A]    = new DirectoryStreamFilter[A] { def accept(entry: A) = p(entry) }
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

trait PrimitiveInstances {
  implicit def boolOrder: Order[Bool]   = orderBy[Bool](x => if (x) 1 else 0)
  implicit def byteOrder: Order[Byte]   = Order.fromInt(_ - _)
  implicit def charOrder: Order[Char]   = Order.fromInt[Char](_ - _)
  implicit def shortOrder: Order[Short] = Order.fromInt[Short](_ - _)
  implicit def intOrder: Order[Int]     = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]   = Order.fromLong[Long](_ - _)
  implicit def unitOrder: Order[Unit]   = Order.fromInt[Unit]((x, y) => 0)
}

trait AlgebraInstances {
  implicit def predicateAlgebra[A] : BooleanAlgebra[ToBool[A]]     = new Algebras.PredicateAlgebra[A]
  implicit def intensionalSetAlgebra[A] : BooleanAlgebra[InSet[A]] = new Algebras.InSetAlgebra[A]
}

trait OrderInstancesLow {
  // If this is written in the obvious way, i.e.
  //
  //   implicit def comparableOrder[A <: Comparable[A]] : Order[A]
  //
  // Then it isn't found for infix operations on Comparables. We also can't call
  // Order.natural[A]() because it has that bound, despite the implicit witness.
  implicit def comparableOrder[A](implicit ev: A <:< Comparable[A]): Order[A] = Order.fromInt[A](_ compareTo _)
}

trait OrderInstances extends OrderInstancesLow {
  // Some unfortunate rocket dentistry necessary here.
  // This doesn't work because scala comes up with "Any" due to the fbound.
  // implicit def enumOrder[A <: jEnum[A]]: Order[A] = Order.fromInt[A](_.ordinal - _.ordinal)
  //
  // This one doesn't work if it's A <:< jEnum[A], but jEnum[_] is just enough to get what we need.
  implicit def enumOrder[A](implicit ev: A <:< jEnum[_]): Order[A] = orderBy[A](_.ordinal) // Order.fromInt[A](_.ordinal - _.ordinal)

  implicit def indexOrder: Order[Index]                                     = orderBy[Index](_.get)
  implicit def preciseOrder[A <: Precise]: Order[A]                         = orderBy[Precise](_.longValue)
  implicit def stringOrder: Order[String]                                   = Order.fromLong[String](_ compareTo _)
  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = orderBy[(A, B)](fst) | snd
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = orderBy[(A, B, C)](_._1) | (_._2) | (_._3)
}

trait EmptyInstances0 {
  implicit def emptyCanBuild[R](implicit z: CanBuild[_, R]): Empty[R] = Empty(z().result)
}

trait EmptyInstances extends EmptyInstances0 {
  implicit def emptyAtomicView[A, Repr] : Empty[AtomicView[A, Repr]] = Empty(View.each[A, Repr](emptyValue))
  implicit def emptyBuilds[R](implicit z: Builds[_, R]): Empty[R]    = Empty(z build Each.empty)
  implicit def emptyOption[A] : Empty[Option[A]]                     = Empty(None)
  implicit def emptyTuple[A: Empty, B: Empty]: Empty[(A, B)]         = Empty(emptyValue[A] -> emptyValue[B])
  implicit def emptyView[A] : Empty[View[A]]                         = Empty(view())

  implicit def emptyFile: Empty[jFile]            = Empty(NoFile)
  implicit def emptyIndex: Empty[Index]           = Empty(NoIndex)
  implicit def emptyIndexRange: Empty[IndexRange] = Empty(indexRange(0, 0))
  implicit def emptyPath: Empty[Path]             = Empty(NoPath)
  implicit def emptyDoc: Empty[Doc]               = Empty(Doc.empty)
  implicit def emptyString: Empty[String]         = Empty("")
}

trait EqInstances extends OrderInstances {
  import StdShow._

  implicit def classWrapperEq: Hash[JavaClass] = inheritEq
  implicit def classEq: Hash[Class[_]]         = inheritEq
  implicit def offsetEq: Hash[Offset]          = inheritEq
  implicit def pathEq: Eq[Path]                = shownEq[Path]
  implicit def sizeEq: Hash[Size]              = inheritEq

  /** The throwableEq defined above conveniently conflicts with the actual
   *  implicit parameter to the method. W... T... F. On top of this the error
   *  message is simply "value === is not a member of Throwable".
   */
  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y // would be x === y, but.
    case _                        => false
  }

  implicit def arrayEq[A: Eq] : Eq[Array[A]]   = eqBy[Array[A]](_.toDirect)
  implicit def vectorEq[A: Eq] : Eq[Direct[A]] = Eq(_ zip _ corresponds (_ === _))
}
