package psp
package std

import java.nio.file.Paths
import java.nio.file.{ attribute => jnfa }
import psp.api._
import psp.ext.ExternalLibs

abstract class StdPackageObject extends scala.AnyRef
      with StdEmpty
      with StdJava
      with PrimitiveInstances
      with AlgebraInstances
      with StdImplicits
      with PspApi {


  // Type aliases I don't like enough to have in the API.
  type Bag[A]               = ExMap[A, Precise]
  type Bool                 = Boolean
  type CanBuild[-Elem, +To] = scala.collection.generic.CanBuildFrom[_, Elem, To]
  type IndexRange           = Consecutive[api.Index]
  type IntRange             = Consecutive[Int]
  type LongRange            = Consecutive[Long]
  type Renderer             = Show[Doc]
  type UnbuildsAs[+A, R]    = Unbuilds[R] { type Elem <: A }
  type View2D[+A]           = View[View[A]]

  // Ugh. XXX
  implicit def promoteSize(x: Long): Precise                   = Size(x)
  implicit def promoteIndex(x: Long): Index                    = Index(x)
  implicit def wrapClass(x: jClass): JavaClass                 = new JavaClassImpl(x)
  implicit def conforms[A] : (A <:< A)                         = new conformance[A]
  implicit def defaultRenderer: FullRenderer                   = new FullRenderer
  implicit def constantPredicate[A](value: Boolean): ToBool[A] = if (value) ConstantTrue else ConstantFalse
  implicit def funToPartialFunction[A, B](f: Fun[A, B]): A ?=> B = new (A ?=> B) {
    def isDefinedAt(x: A) = f isDefinedAt x
    def apply(x: A)       = f(x)
  }
  implicit class DirectOps[A](val xs: Direct[A]) {
    def head: A = apply(0)
    def last: A = apply(lastIndex)
    def tail    = xs.drop(1)
    def init    = xs.dropRight(1)

    def reverse: Direct[A]  = Direct reversed xs
    def apply(i: Index): A  = xs elemAt i
    def indices: IndexRange = indexRange(0, xs.size.getInt)
    def lastIndex: Index    = Index(xs.size.get - 1)  // effectively maps both undefined and zero to no index.

    def containsIndex(index: Index): Boolean = indices containsInt index.getInt

    @inline def foreachIndex(f: Index => Unit): Unit  = if (xs.size.get > 0L) lowlevel.ll.foreachConsecutive(0, lastIndex.getInt, i => f(Index(i)))
    @inline def foreachIntIndex(f: Int => Unit): Unit = if (xs.size.get > 0L) lowlevel.ll.foreachConsecutive(0, lastIndex.getInt, f)
  }

  implicit final class ForeachOps[A](val xs: Foreach[A]) {
    private[this] def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build Each(xs foreach _)
    def trav: scTraversable[A] = to[scTraversable] // flatMap, usually
    def seq: scSeq[A]          = to[scSeq]         // varargs or unapplySeq, usually
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
  def print[A: Show](x: A): Unit   = putOut(render(x))
  def println[A: Show](x: A): Unit = echoOut(render(x))
  def anyprintln(x: Any): Unit     = echoOut(x.any_s)

  def applyIfNonEmpty[A: Eq](x: A)(f: A => A)(implicit z: Empty[A]): A =
    if (x === emptyValue[A]) x else f(x)

  def ??? : Nothing = throw new scala.NotImplementedError

  def classOf[A: CTag](): Class[_ <: A]      = classTag[A].runtimeClass.castTo[Class[_ <: A]]
  def classTag[A: CTag] : CTag[A]            = ?[CTag[A]]
  def classFilter[A: CTag] : Partial[Any, A] = Partial(_.isClass[A], _.castTo[A])

  def transitiveClosure[A: Eq](root: A)(expand: A => Foreach[A]): View[A] = inView { f =>
    def loop(in: View[A], seen: View[A]): Unit = in filterNot seen.contains match {
      case Each() => ()
      case in     => in foreach f ; loop(in flatMap expand, seen ++ in)
    }
    loop(view(root), view())
  }

  @inline def timed[A](elapsed: Long => Unit)(body: => A): A = {
    val start = nanoTime
    val result = body
    elapsed(nanoTime - start)
    result
  }

  def assert(assertion: => Boolean, msg: => Any): Unit =
    if (!assertion) runtimeException("" + msg)

  def abortTrace(msg: String): Nothing               = new RuntimeException(msg) |> (ex => try throw ex finally ex.printStackTrace)
  def bufferMap[A, B: Empty](): scmMap[A, B]         = scmMap[A, B]() withDefaultValue emptyValue[B]
  def indexRange(start: Long, end: Long): IndexRange = Index(start) until end
  def noNull[A](value: A, orElse: => A): A           = if (value == null) orElse else value
  def nullAs[A] : A                                  = null.asInstanceOf[A]
  def option[A](p: Boolean, x: => A): Option[A]      = if (p) Some(x) else None
  def randomPosInt(max: Int): Int                    = scala.util.Random.nextInt(max + 1)
  def leftFormatString[A](n: Int): FormatFun         = new FormatFun(cond(n == 0, "%s", "%%-%ds" format n))

  def fst[A, B](x: A -> B): A          = x._1
  def snd[A, B](x: A -> B): B          = x._2
  def tuple[A, B](x: A -> B): ((A, B)) = x._1 -> x._2
  def swap[A, B](x: A -> B): B -> A    = x._2 -> x._1
  def swap[A, B](x: A, y: B): B -> A   = y -> x

  def make[R](xs: R): RemakeHelper[R]  = new RemakeHelper[R](xs)
  def make0[R] : MakeHelper[R]         = new MakeHelper[R]
  def make1[CC[_]] : MakeHelper1[CC]   = new MakeHelper1[CC]
  def make2[CC[_,_]] : MakeHelper2[CC] = new MakeHelper2[CC]

  def cond[A](p: Bool, thenp: => A, elsep: => A): A = if (p) thenp else elsep
  def inView[A](mf: Suspended[A]): View[A]          = new LinearView(Each(mf))
  def list[A](xs: A*): Plist[A]                     = xs.toPlist
  def rel[K: Eq, V](xs: (K->V)*): ExMap[K, V]       = xs.m.toExMap
  def set[A: Eq](xs: A*): ExSet[A]                  = xs.toExSet
  def vec[@fspec A](xs: A*): Vec[A]                 = xs.toVec
  def view[A](xs: A*): View[A]                      = xs.toVec.m
  def zip[A, B](xs: (A->B)*): ZipView[A, B]         = Zip zip1 xs.m
}
