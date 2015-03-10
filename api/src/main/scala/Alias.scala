package psp
package api

// Importing from this is necessary to use these aliases within the api package,
// where they aren't otherwise visible because there's no api package object.
object Api extends Aliases

// This mostly consists of "magic" types which can never be avoided due to language
// privilege. The rest of the standard type aliases are defined in the dmz package.
trait Aliases extends scala.Any {
  type Array[A]      = scala.Array[A]
  type CTag[A]       = scala.reflect.ClassTag[A]
  type Dynamic       = scala.Dynamic
  type Option[+A]    = scala.Option[A]
  type Product       = scala.Product
  type ScalaNumber   = scala.math.ScalaNumber
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
  type Ref[+A]     = A with AnyRef               // Promotes an A <: Any into an A <: AnyRef
  type ->[+A, +B]  = scala.Product2[A, B]        // A less overconstrained tuple.
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
  type Array2[A]            = Array[Array[A]]
  type Array3[A]            = Array[Array[Array[A]]]
  type Array4[A]            = Array[Array[Array[Array[A]]]]
  type Array5[A]            = Array[Array[Array[Array[Array[A]]]]]
  type Bag[A]               = ExMap[A, Precise]
  type BinOp[A]             = (A, A) => A // binary operation
  type Predicate2[-A1, -A2] = (A1, A2) => Boolean
  type Relation[-A]         = (A, A) => Bool
  type Suspended[+A]        = ToUnit[ToUnit[A]]
  type ToBool[-A]           = A => Bool
  type ToInt[-A]            = A => Int
  type ToSelf[A]            = A => A
  type ToString[-A]         = A => String
  type ToUnit[-A]           = A => Unit

  // You can't use string interpolation without a StringContext term in scope.
  def StringContext = scala.StringContext

  // A few methods it is convenient to expose at this level.
  def ?[A](implicit value: A): A               = value
  def emptyValue[A](implicit z: Empty[A]): A   = z.empty
  def identity[A](x: A): A                     = x
  def implicitly[A](implicit x: A): A          = x
  def longCmp(diff: Long): Cmp                 = if (diff < 0) Cmp.LT else if (diff > 0) Cmp.GT else Cmp.EQ
  def newArray[A: CTag](length: Int): Array[A] = new Array[A](length)
  def none: Option[Nothing]                    = scala.None
  def show[A: Show] : Show[A]                  = ?
  def some[A](x: A): Some[A]                   = scala.Some(x)
}
