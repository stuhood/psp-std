package psp

import java.nio.file.Paths
import java.nio.file.{ attribute => jnfa }
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.api._
import psp.std.StdShow._

package object std extends psp.std.StdPackage with psp.impl.CreateBy {
  /** Scala, so aggravating.
   *  [error] could not find implicit value for parameter equiv: psp.api.Eq[psp.tests.Pint => psp.std.Boolean]
   *  The parameter can be given explicitly, it just won't be found unless the function type is invariant.
   *  The same issue arises with intensional sets.
   */
  type InvariantPredicate[A] = A => Boolean
  type InvariantInSet[A]     = InSet[A]
  type View2D[+A]            = View[View[A]]
  type Each2D[+A]            = Each[Each[A]]
  type IndexRange            = Consecutive[Index]
  type IntRange              = Consecutive[Int]

  // Inlinable.
  final val InputStreamBufferSize = 8192
  final val MaxInt                = scala.Int.MaxValue
  final val MaxLong               = scala.Long.MaxValue
  final val MinInt                = scala.Int.MinValue
  final val MinLong               = scala.Long.MinValue
  final val MaxIndex              = Index(MaxLong)
  final val PositiveInfinity      = scala.Double.PositiveInfinity

  // DMZ.
  final val ::      = psp.dmz.::
  final val Array   = psp.dmz.Array
  final val Console = psp.dmz.Console
  final val Failure = psp.dmz.Failure
  final val Set     = psp.dmz.Set
  final val Success = psp.dmz.Success
  final val System  = psp.dmz.System
  final val Try     = psp.dmz.Try
  final val math    = psp.dmz.math
  final val sys     = psp.dmz.sys

  final val BigDecimal      = scala.math.BigDecimal
  final val BigInt          = scala.math.BigInt
  final val ClassTag        = scala.reflect.ClassTag
  final val NameTransformer = scala.reflect.NameTransformer
  final val Nil             = sci.Nil
  final val None            = scala.None
  final val Option          = scala.Option
  final val Ordering        = scala.math.Ordering
  final val Some            = scala.Some
  final val scIterator      = sc.Iterator
  final val scSeq           = sc.Seq
  final val sciBitSet       = sci.BitSet
  final val sciIndexedSeq   = sci.IndexedSeq
  final val sciIterable     = sci.Iterable
  final val sciLinearSeq    = sci.LinearSeq
  final val sciList         = sci.List
  final val sciMap          = sci.Map
  final val sciNumericRange = sci.NumericRange
  final val sciRange        = sci.Range
  final val sciSeq          = sci.Seq
  final val sciSet          = sci.Set
  final val sciStream       = sci.Stream
  final val sciTraversable  = sci.Traversable
  final val sciVector       = sci.Vector
  final val scmMap          = scm.Map
  final val scmSeq          = scm.Seq
  final val scmSet          = scm.Set

  final val ConstantTrue  = (x: Any) => true
  final val ConstantFalse = (x: Any) => false
  final val CTag          = scala.reflect.ClassTag
  final val EOL           = sysprop.lineSeparator
  final val NoFile: jFile = jFile("")
  final val NoPath: Path  = path("")
  final val NoUri: jUri   = jUri("")
  final val NoIndex       = Index.undefined
  final val NoNth         = Nth.undefined
  final val ->            = psp.api.Pair

  // Methods similar to the more useful ones in scala's Predef.
  def ??? : Nothing                                                                        = throw new scala.NotImplementedError
  def assert(assertion: => Boolean)(implicit z: Assertions): Unit                          = Assertions.using(z)(assertion, "assertion failed")
  def assert(assertion: => Boolean, msg: => Any)(implicit z: Assertions): Unit             = Assertions.using(z)(assertion, s"assertion failed: $msg")
  def asserting[A](x: A)(assertion: => Boolean, msg: => String)(implicit z: Assertions): A = x sideEffect assert(assertion, msg)
  def printResult[A: TryShow](msg: String)(result: A): A                                   = result doto (r => println(pp"$msg: $r"))
  def printResultIf[A: TryShow : Eq](show: A, msg: String)(result: A): A                   = result doto (r => if (r === show) println(pp"$msg: $r"))
  def print[A: TryShow](x: A): Unit                                                        = Console putOut pp"$x"
  def println[A: TryShow](x: A): Unit                                                      = Console echoOut pp"$x"
  def require(requirement: Boolean): Unit                                                  = if (!requirement) illegalArgumentException("requirement failed")
  def require(requirement: Boolean, msg: => Any): Unit                                     = if (!requirement) illegalArgumentException(s"requirement failed: $msg")
  def showResult[A: TryShow](msg: String)(result: A): A                                    = result doto (r => println(pp"$msg: $r"))

  def echoErr(x: Shown): Unit = Console echoErr x.to_s
  def echoOut(x: Shown): Unit = Console echoOut x.to_s

  // Operations involving classes, classpaths, and classloaders.
  def classLoaderOf[A: CTag](): ClassLoader = classOf[A].getClassLoader
  def classOf[A: CTag](): Class[_ <: A]     = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classTag[T: CTag] : CTag[T]           = implicitly[CTag[T]]
  def loaderOf[A: CTag] : ClassLoader       = noNull(classLoaderOf[A], nullLoader)
  def nullLoader(): ClassLoader             = NullClassLoader
  def findLoader(): Option[ClassLoader]     = noNull(contextClassLoader, loaderOf[this.type]) |> (x => option(x ne null, x))
  def pClassOf[A: CTag](): JavaClass        = new JavaClassImpl(classOf[A])

  def resourceNames(root: Path): Direct[String] = findLoader.fold(direct[String]())(cl => Resources.getResourceNames(cl, root).toDirect)
  def resource(name: String): Array[Byte]       = findLoader.fold(Array.empty[Byte])(_ getResourceAsStream name slurp)
  def resourceString(name: String): String      = utf8(resource(name)).to_s
  def classFilter[A: CTag] : Any ?=> A          = newPartial(_.isClass[A], _.castTo[A])
  def path(s: String): Path                     = Paths get s
  def callable[A](body: => A): jCallable[A]     = new java.util.concurrent.Callable[A] { def call(): A = body }

  // Operations involving encoding/decoding of string data.
  def utf8(xs: Array[Byte]): Utf8   = new Utf8(xs)
  def decodeName(s: String): String = s.mapSplit('.')(NameTransformer.decode)
  def encodeName(s: String): String = s.mapSplit('.')(NameTransformer.encode)

  // Operations involving time and date.
  def formattedDate(format: String)(date: jDate): String = new java.text.SimpleDateFormat(format) format date
  def dateTime(): String                                 = formattedDate("yyyyMMdd-HH-mm-ss")(new jDate)
  def now(): FileTime                                    = jnfa.FileTime fromMillis milliTime
  def timed[A](body: => A): A                            = nanoTime |> (start => try body finally echoErr("Elapsed: %.3f ms" format (nanoTime - start) / 1e6))

  // Operations involving Null, Nothing, and casts.
  def abortTrace(msg: String): Nothing     = new RuntimeException(msg) |> (ex => try throw ex finally ex.printStackTrace)
  def abort(msg: String): Nothing          = runtimeException(msg)
  def noNull[A](value: A, orElse: => A): A = if (value == null) orElse else value
  def nullAs[A] : A                        = null.asInstanceOf[A]

  def andFalse(x: Unit, xs: Unit*): Boolean        = false
  def andResult[A](x: A, xs: Unit*): A             = x
  def andTrue(x: Unit, xs: Unit*): Boolean         = true
  def direct[A](xs: A*): Direct[A]                 = Direct fromScala xs
  def each[A](xs: sCollection[A]): Each[A]         = Each fromScala xs
  def fullIndexRange: IndexRange                   = indexRange(0, MaxInt)
  def indexRange(start: Int, end: Int): IndexRange = Consecutive.until(start, end, Index(_))
  def intRange(start: Int, end: Int): IntRange     = Consecutive.until(start, end)
  def nthRange(start: Int, end: Int): IntRange     = Consecutive.to(start, end)
  def nullStream(): InputStream                    = NullInputStream
  def offset(x: Int): Offset                       = Offset(x)
  def option[A](p: Boolean, x: => A): Option[A]    = if (p) Some(x) else None
  def partial[A, B](f: A ?=> B): A ?=> B           = f
  def regex(re: String): Regex                     = Regex(re)

  def andClose[A <: Closeable, B](x: A)(f: A => B): B = try f(x) finally x.close()

  def spawn[A](body: => A): Unit = {
    val t = new Thread() { override def run(): Unit = body }
    t setDaemon true
    t.start()
  }

  // Java.
  def jConcurrentMap[K, V](xs: (K -> V)*): jConcurrentMap[K, V] = new jConcurrentHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jFile(s: String): jFile                                   = path(s).toFile
  def jList[A](xs: A*): jList[A]                                = java.util.Arrays.asList(xs: _*)
  def jMap[K, V](xs: (K -> V)*): jMap[K, V]                     = new jHashMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))
  def jSet[A](xs: A*): jSet[A]                                  = new jHashSet[A] doto (b => xs foreach b.add)
  def jUri(x: String): jUri                                     = java.net.URI create x
  def jUrl(x: String): jUrl                                     = jUri(x).toURL
  def jIdMap[K, V](xs: (K -> V)*): jIdMap[K, V]                 = new jIdMap[K, V] doto (b => for ((k, v) <- xs) b.put(k, v))

  def fst[A, B](x: A -> B): A = x._1
  def snd[A, B](x: A -> B): B = x._2

  def exMap[K: HashEq, V](xs: (K -> V)*): ExMap[K, V]   = xs.m.toExMap
  def exSeq[A](xs: A*): Each[A]                         = xs.m.toEach
  def exSet[A: HashEq](xs: A*): ExSet[A]                = xs.m.toExSet
  def exView[A](xs: A*): View[A]                        = Direct[A](xs: _*).m
  def inMap[K, V](p: ToBool[K], f: K => V): InMap[K, V] = InMap(inSet(p), f)
  def inSet[A](p: ToBool[A]): InSet[A]                  = InSet(p)
  def inView[A](mf: Suspended[A]): View[A]              = Each(mf).m
  def mutableMap[K, V](xs: (K -> V)*): MutableMap[K, V] = MutableMap(jConcurrentMap(xs: _*))
  def zipView[A, B](xs: (A, B)*): ZipView[A, B]         = Zipped1(xs.seq)

  def randomNat(max: Int): Int                              = scala.util.Random.nextInt(max)
  def bufferMap[A, B: Empty](): scmMap[A, B]                = scmMap[A, B]() withDefaultValue emptyValue[B]
  def newPartial[K, V](p: K => Boolean, f: K => V): K ?=> V = { case x if p(x) => f(x) }
  def newSize(n: Long): Precise                             = if (n < 0) Precise(0) else if (n > MaxInt) Precise(n) else Precise(n.toInt)
}
