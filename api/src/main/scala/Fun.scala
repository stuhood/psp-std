package psp
package api

import scala.{ Any, AnyVal, Boolean, PartialFunction, Nothing, StringContext, Option, None, Some, Unit  }

/** A richer function abstraction.
 *  I think this can replace a lot.
 */
sealed trait Fun[-A, +B]                                         extends Any
final case class Opaque[-A, +B](f: A => B)                       extends Fun[A, B]
final case class Defaulted[-A, +B](g: A => B, u: Fun[A, B])      extends Fun[A, B]
final case class FilterIn[-A, +B](p: A => Boolean, u: Fun[A, B]) extends Fun[A, B]
final case class MapIn[-A0, A, +B](g: A0 => A, u: Fun[A, B])     extends Fun[A0, B]
final case class MapOut[-A, B, +B1](g: B => B1, u: Fun[A, B])    extends Fun[A, B1]

// TODO
// final case class Composed[-A, B, +C](f: Fun[A, B], g: Fun[B, C]) extends Fun[A, C]

object Fun {
  private def undefined(x: Any) = throw new java.lang.RuntimeException(s"Undefined function called with argument $x")

  val Empty = apply[Any, Nothing](undefined) filterIn (_ => false)

  def partial[A, B](pf: PartialFunction[A, B]): Fun[A, B] = apply(pf) filterIn pf.isDefinedAt
  def apply[A, B](f: A => B): Fun[A, B]                   = Opaque(f)

  implicit class FunOps[A, B](val f: Fun[A, B]) extends AnyVal {
    outer =>

    def toPartial: PartialFunction[A, B] = new PartialFunction[A, B] {
      def isDefinedAt(x: A) = outer isDefinedAt x
      def apply(x: A)       = outer apply x
    }
    def opaquely: Opaque[A, B] = f match {
      case x: Opaque[_, _] => x
      case _               => Opaque(x => if (isDefinedAt(x)) apply(x) else undefined(x))
    }
    def isDefinedAt(x: A): Boolean = f match {
      case Opaque(_)       => true
      case FilterIn(p, u)  => p(x) && (u isDefinedAt x)
      case MapIn(g, u)     => u isDefinedAt g(x)
      case MapOut(_, u)    => u isDefinedAt x
      case Defaulted(_, u) => u isDefinedAt x
    }
    def apply(x: A): B = f match {
      case Opaque(g)       => g(x)
      case Defaulted(g, u) => if (u isDefinedAt x) u(x) else g(x)
      case FilterIn(_, u)  => u(x) // filter is checked at isDefinedAt
      case MapIn(g, u)     => u(g(x))
      case MapOut(g, u)    => g(u(x))
    }
    def get(x: A): Option[B] = f match {
      case Opaque(g)       => Some(g(x))
      case Defaulted(_, u) => u get x // get ignores default
      case FilterIn(p, u)  => if (p(x)) u get x else None
      case MapIn(g, u)     => u get g(x)
      case MapOut(g, u)    => u get x map g
    }
    def defaulted(g: A => B): Defaulted[A, B] = f match {
      case Defaulted(_, u) => Defaulted(g, u)
      case _               => Defaulted(g, f)
    }
    def mapOut[B1](g: B => B1): Fun[A, B1] = f match {
      case MapOut(g0, u) => MapOut(g0 andThen g, u)
      case _             => MapOut(g, f)
    }
    def mapIn[A1](g: A1 => A): Fun[A1, B] = f match {
      case MapIn(g0, u) => MapIn(g andThen g0, u)
      case _            => MapIn(g, f)
    }
    def filterIn(p: A => Boolean): FilterIn[A, B] = f match {
      case FilterIn(p0, u) => FilterIn(x => p0(x) && p(x), u)
      case _               => FilterIn(p, f)
    }

    def traced(in: A => Unit, out: B => Unit): Fun[A, B] = ( f
       mapIn[A] { x => in(x) ; x }
      mapOut[B] { x => out(x) ; x }
    )
    def memoized: Fun[A, B] = {
      val cache = scala.collection.mutable.Map[A, B]()
      Opaque[A, B](x => cache.getOrElseUpdate(x, f(x))) filterIn (x => (cache contains x) || (f isDefinedAt x))
    }
  }
}
