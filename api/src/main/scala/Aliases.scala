package psp
package api

import scala.{ collection => sc }

// import scala.specialized

// Importing from this is necessary to use these aliases within the api package,
// where they aren't otherwise visible because there's no api package object.
object Api extends Aliases
//   type spec     = specialized
//   val SpecTypes = new scala.Specializable.Group((scala.Int, scala.Long, scala.Double))
// }

trait Aliases extends psp.ext.ScalaLib with psp.ext.JavaLib {
  // Irregularly named.
  type CanBuild[-Elem, +To] = sc.generic.CanBuildFrom[_, Elem, To]
  type GTOnce[+A]           = sc.GenTraversableOnce[A]
  type sCollection[+A]      = sc.GenTraversable[A]

  // Caveat: ?=> associates to the left instead of the right.
  type Ref[+A]     = AnyRef with A               // Promotes an A <: Any into an A <: AnyRef.
  type Id[X]       = X                           // The identity type constructor.
  type ->[+A, +B]  = scala.Product2[A, B]        // A less overconstrained product type.
  type ?=>[-A, +B] = scala.PartialFunction[A, B] // Less clumsy syntax for the all-important partial function.

  // Aliases for common function types.
  type BinOp[A]          = (A, A) => A // binary operation
  type OrderRelation[-A] = (A, A) => Cmp
  type Relation[-A]      = (A, A) => Bool
  type Suspended[+A]     = ToUnit[ToUnit[A]]
  type ToBool[-A]        = A => Bool
  type ToInt[-A]         = A => Int
  type ToSelf[A]         = A => A
  type ToString[-A]      = A => String
  type ToUnit[-A]        = A => Unit

  // Other type aliases.
  type Array0D[A]        = A
  type Array1D[A]        = Array[A]
  type Array2D[A]        = Array[Array[A]]
  type Array3D[A]        = Array[Array[Array[A]]]
  type Array4D[A]        = Array[Array[Array[Array[A]]]]
  type Array5D[A]        = Array[Array[Array[Array[Array[A]]]]]
  type Array6D[A]        = Array[Array[Array[Array[Array[Array[A]]]]]]
  type Bag[A]            = ExMap[A, Precise]
  type Bool              = Boolean
  type Renderer          = Show[Doc]
  type UShort            = Char     // unsigned short
  type UnbuildsAs[+A, R] = Unbuilds[R] { type Elem <: A }
  type View2D[+A]        = View[View[A]]

  // A few methods it is convenient to expose at this level.
  def ?[A](implicit value: A): A               = value
  def abort(msg: String): Nothing              = throw new java.lang.RuntimeException(msg)
  def emptyValue[A](implicit z: Empty[A]): A   = z.empty
  def identity[A](x: A): A                     = x
  def implicitly[A](implicit x: A): A          = x
  def none[A](): Option[A]                     = scala.None
  def show[A](implicit z: Show[A]): Show[A]    = z
  def some[A](x: A): Some[A]                   = scala.Some(x)
  def sideEffect[A](result: A, exprs: Any*): A = result

  def newArray[A: CTag](length: Int): Array[A] = new Array[A](length)
  def copyArray[A: CTag](src: Array[A]): Array[A] = {
    val target = newArray[A](src.length)
    sideEffect(target, arraycopy(src, 0, target, 0, src.length))
  }

  def arraycopy[A](src: Array[A], srcPos: Int, dest: Array[A], destPos: Int, len: Int): Unit =
    java.lang.System.arraycopy(src, srcPos, dest, destPos, len)
}
