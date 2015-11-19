package psp

import java.nio.file.Paths
import java.nio.file.{ attribute => jnfa }
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import psp.api._
import Api.SpecTypes
import psp.dmz.Console

package object std extends psp.std.StdPackage {
  import Unsafe.inheritedShow
  private implicit def lexical = lexicalOrder

  type IndexRange      = Consecutive[Index]
  type IntRange        = Consecutive[Int]
  type BuildsMap[K, V] = Builds[K -> V, ExMap[K, V]]

  def lexicalOrder: Order[String] = Order.fromInt(_ compareTo _)

  def inheritShow[A] : Show[A]           = Show.Inherited
  def inheritEq[A] : Hash[A]             = Eq.Inherited
  def referenceEq[A <: AnyRef] : Hash[A] = Eq.Reference
  def stringEq[A] : Hash[A]              = Eq.ToString
  def shownEq[A: Show] : Hash[A]         = inheritEq[String] on (_.render)

  implicit def opsFun[A, B](f: Fun[A, B]): ops.FunOps[A, B] = new ops.FunOps(f)
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B = new (A ?=> B) {
    def isDefinedAt(x: A) = f isDefinedAt x
    def apply(x: A)       = f(x)
  }

  // Helpers for inference when calling 'on' on contravariant type classes.
  def eqBy[A]    = new EqBy[A]
  def orderBy[A] = new OrderBy[A]
  def showBy[A]  = new ShowBy[A]

  // DMZ.
  final val ::      = psp.dmz.::
  final val Array   = psp.dmz.Array
  final val Failure = psp.dmz.Failure
  final val Success = psp.dmz.Success
  final val Try     = psp.dmz.Try

  final val NameTransformer = scala.reflect.NameTransformer
  final val Nil             = sci.Nil
  final val None            = scala.None
  final val Option          = scala.Option
  final val Some            = scala.Some
  final val sciList         = sci.List
  final val sciMap          = sci.Map
  final val sciSeq          = sci.Seq
  final val sciSet          = sci.Set
  final val sciVector       = sci.Vector
  final val scmMap          = scm.Map

  final val MaxIndex      = Index(MaxLong)
  final val NoFile: jFile = new jFile("")
  final val NoPath: Path  = path("")
  final val NoUri: jUri   = jUri("")
  final val NoIndex       = Index.undefined
  final val NoNth         = Nth.undefined
  final val ->            = psp.api.Pair

  implicit val defaultRenderer: FullRenderer              = new FullRenderer
  implicit def docOrder(implicit z: Renderer): Order[Doc] = orderBy[Doc](z show _)

  implicit class DocOps(val lhs: Doc) {
    def doc: Doc                             = lhs
    def render(implicit z: Renderer): String = z show lhs
    def isEmpty: Boolean                     = lhs eq emptyValue[Doc]

    def ~(rhs: Doc): Doc   = Doc.Cat(lhs, rhs)
    def <>(rhs: Doc): Doc  = if (lhs.isEmpty) rhs else if (rhs.isEmpty) lhs else lhs ~ rhs
    def <+>(rhs: Doc): Doc = if (lhs.isEmpty) rhs else if (rhs.isEmpty) lhs else lhs ~ " ".s ~ rhs
  }

  def print[A: Show](x: A): Unit   = Console putOut show"$x"
  def println[A: Show](x: A): Unit = Console echoOut show"$x"
  def anyprintln(x: Any): Unit     = Console echoOut x.any_s

  def ??? : Nothing                                                            = throw new scala.NotImplementedError
  def assert(assertion: => Boolean, msg: => Any)(implicit z: Assertions): Unit = Assertions.using(z)(assertion, s"assertion failed: $msg")

  def classOf[A: CTag](): Class[_ <: A]     = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classTag[A: CTag] : CTag[A]           = implicitly[CTag[A]]
  def path(s: String): Path                 = Paths get s

  // Operations involving encoding/decoding of string data.
  def utf8(xs: Array[Byte]): Utf8   = new Utf8(xs)
  def decodeName(s: String): String = s.mapSplit('.')(NameTransformer.decode)
  def encodeName(s: String): String = s.mapSplit('.')(NameTransformer.encode)

  // Operations involving time and date.
  def formattedDate(format: String)(date: jDate): String = new java.text.SimpleDateFormat(format) format date
  def dateTime(): String                                 = formattedDate("yyyyMMdd-HH-mm-ss")(new jDate)
  def now(): FileTime                                    = jnfa.FileTime fromMillis milliTime

  @inline def timed[A](elapsed: Long => Unit)(body: => A): A = {
    val start = nanoTime
    val result = body
    elapsed(nanoTime - start)
    result
  }

  def abort(msg: String): Nothing                           = runtimeException(msg)
  def abortTrace(msg: String): Nothing                      = new RuntimeException(msg) |> (ex => try throw ex finally ex.printStackTrace)
  def andClose[A <: Closeable, B](x: A)(f: A => B): B       = try f(x) finally x.close()
  def andFalse(x: Unit, xs: Unit*): Boolean                 = false
  def andTrue(x: Unit, xs: Unit*): Boolean                  = true
  def bufferMap[A, B: Empty](): scmMap[A, B]                = scmMap[A, B]() withDefaultValue emptyValue[B]
  def fullIndexRange: IndexRange                            = indexRange(0, MaxInt)
  def indexRange(start: Int, end: Int): IndexRange          = Consecutive.until(start, end, Index(_))
  def intRange(start: Int, end: Int): IntRange              = Consecutive.until(start, end)
  def newPartial[K, V](p: K => Boolean, f: K => V): K ?=> V = { case x if p(x) => f(x) }
  def noNull[A](value: A, orElse: => A): A                  = if (value == null) orElse else value
  def nullAs[A] : A                                         = null.asInstanceOf[A]
  def option[A](p: Boolean, x: => A): Option[A]             = if (p) Some(x) else None
  def randomNat(max: Int): Int                              = scala.util.Random.nextInt(max)

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

  def view[A](xs: A*): View[A]                  = xs.toVec.m
  def vec[@spec(SpecTypes) A](xs: A*): Vec[A]   = xs.toVec
  def set[A: Eq](xs: A*): ExSet[A]              = xs.toExSet
  def rel[K: Eq, V](xs: (K->V)*): ExMap[K, V]   = xs.m.toExMap
  def list[A](xs: A*): Plist[A]                 = xs.toPlist
  def inView[A](mf: Suspended[A]): View[A]      = Each(mf).m
  def zipView[A, B](xs: (A, B)*): ZipView[A, B] = Zipped1(xs.seq)
}

package std {
  object sys {
    def error(msg: java.lang.String): Nothing = scala.sys.error(msg)
    def props                                 = scala.sys.props
    def env                                   = scala.sys.env
  }
}
