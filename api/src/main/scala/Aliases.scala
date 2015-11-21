package psp
package api

import scala.specialized

// Importing from this is necessary to use these aliases within the api package,
// where they aren't otherwise visible because there's no api package object.
object Api extends Aliases {
  type spec     = specialized
  val SpecTypes = new scala.Specializable.Group((scala.Int, scala.Long, scala.Double))
}

// This mostly consists of "magic" types which can never be avoided due to language
// privilege. The rest of the standard type aliases are defined in the dmz package.
trait Aliases extends scala.Any {
  type Array[A]      = scala.Array[A]
  type CTag[A]       = scala.reflect.ClassTag[A]
  type Dynamic       = scala.Dynamic
  type Option[+A]    = scala.Option[A]
  type Some[+A]      = scala.Some[A]
  type String        = java.lang.String
  type StringContext = scala.StringContext

  // The top and bottom types.
  type Any     = scala.Any
  type AnyRef  = scala.AnyRef
  type AnyVal  = scala.AnyVal
  type Null    = scala.Null
  type Nothing = scala.Nothing

  // Missing pieces of scala-library.
  // Caveat: ?=> associates to the left instead of the right.
  type Ref[+A]     = AnyRef with A               // Promotes an A <: Any into an A <: AnyRef.
  type Id[X]       = X                           // The identity type constructor.
  type ->[+A, +B]  = scala.Product2[A, B]        // A less overconstrained product type.
  type ?=>[-A, +B] = scala.PartialFunction[A, B] // Less clumsy syntax for the all-important partial function.

  // The eight primitive types of the jvm, plus the scala version of void.
  type Boolean = scala.Boolean
  type Byte    = scala.Byte
  type Char    = scala.Char
  type Double  = scala.Double
  type Float   = scala.Float
  type Int     = scala.Int
  type Long    = scala.Long
  type Short   = scala.Short
  type Unit    = scala.Unit

  // Better names for primitive types.
  // TODO: experiment with removing the "real" primitives entirely.
  type Bool   = Boolean
  type UShort = Char     // unsigned short

  // Original type aliases.
  type Array0D[A]        = A
  type Array1D[A]        = Array[A]
  type Array2D[A]        = Array[Array[A]]
  type Array3D[A]        = Array[Array[Array[A]]]
  type Array4D[A]        = Array[Array[Array[Array[A]]]]
  type Array5D[A]        = Array[Array[Array[Array[Array[A]]]]]
  type Array6D[A]        = Array[Array[Array[Array[Array[Array[A]]]]]]
  type Bag[A]            = ExMap[A, Precise]
  type BinOp[A]          = (A, A) => A // binary operation
  type OrderRelation[-A] = (A, A) => Cmp
  type Relation[-A]      = (A, A) => Bool
  type Renderer          = Show[Doc]
  type Suspended[+A]     = ToUnit[ToUnit[A]]
  type ToBool[-A]        = A => Bool
  type ToInt[-A]         = A => Int
  type ToSelf[A]         = A => A
  type ToString[-A]      = A => String
  type ToUnit[-A]        = A => Unit
  type UnbuildsAs[+A, R] = Unbuilds[R] { type Elem <: A }
  type View2D[+A]        = View[View[A]]

  // You can't use string interpolation without a StringContext term in scope.
  def StringContext = scala.StringContext

  // A few methods it is convenient to expose at this level.
  def ?[A](implicit value: A): A             = value
  def abort(msg: String): Nothing            = throw new java.lang.RuntimeException(msg)
  def emptyValue[A](implicit z: Empty[A]): A = z.empty
  def identity[A](x: A): A                   = x
  def implicitly[A](implicit x: A): A        = x
  def none[A](): Option[A]                   = scala.None
  def show[A](implicit z: Show[A]): Show[A]  = z
  def some[A](x: A): Some[A]                 = scala.Some(x)

  def newArray[A: CTag](length: Int): Array[A] = new Array[A](length)
  def copyArray[A: CTag](src: Array[A]): Array[A] = {
    val len    = src.length
    val target = newArray[A](len)
    java.lang.System.arraycopy(src, 0, target, 0, len)
    target
  }
}
