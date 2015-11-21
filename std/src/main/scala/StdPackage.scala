package psp
package std

import java.nio.file.Paths
import java.nio.file.{ attribute => jnfa }
import psp.api._
import scala.reflect.NameTransformer

abstract class StdPackageObject extends scala.AnyRef
      with EmptyInstances
      with PrimitiveInstances
      with AlgebraInstances
      with GlobalShow
      with StdImplicits
      with Aliases
      with psp.dmz.ScalaDmz
      with psp.dmz.JavaDmz {

  // Ugh. XXX
  implicit def promoteSize(x: Int): Precise                 = Size(x)
  implicit def opsFun[A, B](f: Fun[A, B]): ops.FunOps[A, B] = new ops.FunOps(f)
  implicit def wrapClass(x: jClass): JavaClass              = new JavaClassImpl(x)
  implicit def conforms[A] : (A <:< A)                      = new conformance[A]

  implicit def constantPredicate[A](value: Boolean): ToBool[A] = if (value) ConstantTrue else ConstantFalse
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B = new (A ?=> B) {
    def isDefinedAt(x: A) = f isDefinedAt x
    def apply(x: A)       = f(x)
  }
  implicit class DirectOps[A](val xs: Direct[A]) {
    def apply(i: Index): A                   = xs elemAt i
    def indices: IndexRange                  = indexRange(0, xs.size.getInt)
    def lastIndex: Index                     = Index(xs.size.get - 1)  // effectively maps both undefined and zero to no index.
    def containsIndex(index: Index): Boolean = indices containsInt index.getInt

    @inline def foreachIndex(f: Index => Unit): Unit  = if (xs.size.get > 0L) lowlevel.ll.foreachConsecutive(0, lastIndex.getInt, i => f(Index(i)))
    @inline def foreachIntIndex(f: Int => Unit): Unit = if (xs.size.get > 0L) lowlevel.ll.foreachConsecutive(0, lastIndex.getInt, f)
  }

  def lexicalOrder: Order[String] = Order.fromInt(_ compareTo _)

  def inheritShow[A] : Show[A]           = Show.Inherited
  def inheritEq[A] : Hash[A]             = Eq.Inherited
  def referenceEq[A <: AnyRef] : Hash[A] = Eq.Reference
  def stringEq[A] : Hash[A]              = Eq.ToString
  def shownEq[A: Show] : Hash[A]         = hashBy[A](x => render(x))(Eq.ToString)

  // Helpers for inference when calling 'on' on contravariant type classes.
  def eqBy[A]    = new EqBy[A]
  def orderBy[A] = new OrderBy[A]
  def showBy[A]  = new ShowBy[A]
  def hashBy[A]  = new HashBy[A]

  private def stdout                  = scala.Console.out
  private def putOut(msg: Any): Unit  = sideEffect(stdout print msg, stdout.flush())
  private def echoOut(msg: Any): Unit = stdout println msg

  def render[A](x: A)(implicit z: Show[A]): String = z show x
  def print[A: Show](x: A): Unit   = putOut(show"$x")
  def println[A: Show](x: A): Unit = echoOut(show"$x")
  def anyprintln(x: Any): Unit     = echoOut(x.any_s)

  def applyIfNonEmpty[A](x: A)(f: A => A)(implicit z: Empty[A]): A =
    if (z isEmptyValue x) x else f(x)

  def ??? : Nothing = throw new scala.NotImplementedError

  def assert(assertion: => Boolean, msg: => Any)(implicit z: Assertions): Unit =
    Assertions.using(z)(assertion, s"assertion failed: $msg")

  def classOf[A: CTag](): Class[_ <: A]      = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classTag[A: CTag] : CTag[A]            = implicitly[CTag[A]]
  def classFilter[A: CTag] : Partial[Any, A] = Partial(_.isClass[A], _.castTo[A])
  def jPath(path: String): jPath             = Paths get path
  def jFile(path: String): jFile             = new jFile(path)

  def transitiveClosure[A: Eq](root: A)(expand: A => Foreach[A]): View[A] = inView { f =>
    def loop(in: View[A], seen: View[A]): Unit = in filterNot seen.contains match {
      case Each() => ()
      case in     => in foreach f ; loop(in flatMap expand, seen ++ in)
    }
    loop(view(root), view())
  }

  // Operations involving encoding/decoding of string data.
  def utf8(xs: Array[Byte]): Utf8   = new Utf8(xs)
  def decodeName(s: String): String = s.mapSplit('.')(NameTransformer.decode)
  def encodeName(s: String): String = s.mapSplit('.')(NameTransformer.encode)

  @inline def timed[A](elapsed: Long => Unit)(body: => A): A = {
    val start = nanoTime
    val result = body
    elapsed(nanoTime - start)
    result
  }

  def sortInPlace[A](xs: Array[A]): Array[A] = sideEffect(xs, xs match {
    case xs: Array[Byte]   => java.util.Arrays.sort(xs)
    case xs: Array[Char]   => java.util.Arrays.sort(xs)
    case xs: Array[Short]  => java.util.Arrays.sort(xs)
    case xs: Array[Int]    => java.util.Arrays.sort(xs)
    case xs: Array[Long]   => java.util.Arrays.sort(xs)
    case xs: Array[Double] => java.util.Arrays.sort(xs)
    case xs: Array[Float]  => java.util.Arrays.sort(xs)
    case xs: Array[AnyRef] => java.util.Arrays.sort[AnyRef](xs, Eq.refComparator)
    case _                 =>
  })

  def tabular[A](xs: View[A])(columns: ToString[A]*): String =
    if (xs.nonEmpty && columns.nonEmpty) FunctionGrid(xs.toVec, columns.m).render(inheritShow) else ""

  def abort(msg: String): Nothing                      = runtimeException(msg)
  def abortTrace(msg: String): Nothing                 = new RuntimeException(msg) |> (ex => try throw ex finally ex.printStackTrace)
  def andClose[A <: jCloseable, B](x: A)(f: A => B): B = try f(x) finally x.close()
  def bufferMap[A, B: Empty](): scmMap[A, B]           = scmMap[A, B]() withDefaultValue emptyValue[B]
  def fullIndexRange: IndexRange                       = indexRange(0, MaxInt)
  def indexRange(start: Int, end: Int): IndexRange     = Consecutive.until(start, end, Index(_))
  def intRange(start: Int, end: Int): IntRange         = Consecutive.until(start, end)
  def longRange(start: Long, end: Long): LongRange     = intRange(start.safeInt, end.safeInt) map (_.toLong)
  def noNull[A](value: A, orElse: => A): A             = if (value == null) orElse else value
  def nullAs[A] : A                                    = null.asInstanceOf[A]
  def option[A](p: Boolean, x: => A): Option[A]        = if (p) Some(x) else None
  def randomPosInt(max: Int): Int                      = scala.util.Random.nextInt(max + 1)
  def sideEffect[A](result: A, exprs: Any*): A         = result
  def leftFormatString[A](n: Int): FormatFun           = new FormatFun(cond(n == 0, "%s", "%%-%ds" format n))

  def max(l: Int, r: Int): Int     = if (l >= r) l else r
  def max(l: Long, r: Long): Long  = if (l >= r) l else r
  def max[A: Order](l: A, r: A): A = if (l >= r) l else r
  def min(l: Int, r: Int): Int     = if (l <= r) l else r
  def min(l: Long, r: Long): Long  = if (l <= r) l else r
  def min[A: Order](l: A, r: A): A = if (l <= r) l else r

  // Java.
  def jConcurrentMap[K, V](xs: (K -> V)*): jConcurrentMap[K, V] = new jConcurrentHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jList[A](xs: A*): jList[A]                                = java.util.Arrays.asList(xs: _*)
  def jMap[K, V](xs: (K -> V)*): jMap[K, V]                     = new jHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jSet[A](xs: A*): jSet[A]                                  = new jHashSet[A] doto (b => xs foreach b.add)
  def jUri(x: String): jUri                                     = java.net.URI create x
  def jUrl(x: String): jUrl                                     = jUri(x).toURL

  def fst[A, B](x: A -> B): A          = x._1
  def snd[A, B](x: A -> B): B          = x._2
  def tuple[A, B](x: A -> B): ((A, B)) = x._1 -> x._2
  def swap[A, B](x: A -> B): B -> A    = x._2 -> x._1
  def swap[A, B](x: A, y: B): B -> A   = y -> x

  def cond[A](p: Bool, thenp: => A, elsep: => A): A = if (p) thenp else elsep
  def view[A](xs: A*): View[A]                      = xs.toVec.m
  def vec[@fspec A](xs: A*): Vec[A]                 = xs.toVec
  def set[A: Eq](xs: A*): ExSet[A]                  = xs.toExSet
  def rel[K: Eq, V](xs: (K->V)*): ExMap[K, V]       = xs.m.toExMap
  def list[A](xs: A*): Plist[A]                     = xs.toPlist
  def inView[A](mf: Suspended[A]): View[A]          = Each(mf).m
  def zipView[A, B](xs: (A, B)*): ZipView[A, B]     = Zip zip1 xs.m
}
