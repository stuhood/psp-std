package psp
package api

// This mostly consists of "magic" types which cannot be avoided due to language privilege.
trait ApiAliases extends scala.Any {
  type Array[A]      = scala.Array[A]
  type CTag[A]       = scala.reflect.ClassTag[A]
  type Dynamic       = scala.Dynamic
  type Product       = scala.Product
  type ScalaNumber   = scala.math.ScalaNumber
  type String        = java.lang.String
  type StringContext = scala.StringContext

  // The top types.
  type Any    = scala.Any
  type AnyRef = scala.AnyRef
  type AnyVal = scala.AnyVal

  // The bottom types.
  type Null    = scala.Null
  type Nothing = scala.Nothing

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

  // Original type aliases.
  type ->[+A, +B]           = scala.Product2[A, B]
  type ?=>[-A, +B]          = scala.PartialFunction[A, B]
  type Array2[A]            = Array[Array[A]]
  type Array3[A]            = Array[Array[Array[A]]]
  type Array4[A]            = Array[Array[Array[Array[A]]]]
  type Array5[A]            = Array[Array[Array[Array[Array[A]]]]]
  type Bag[A]               = ExMap[A, Precise]
  type BinOp[A]             = (A, A) => A // binary operation
  type Bool                 = scala.Boolean
  type Bytes                = Array[Byte]
  type Chars                = Array[Char]
  type Predicate2[-A1, -A2] = (A1, A2) => Boolean
  type Ref[+A]              = A with AnyRef
  type Relation[-A]         = (A, A) => Bool
  type Suspended[+A]        = ToUnit[ToUnit[A]]
  type ToBool[-A]           = A => Bool
  type ToInt[-A]            = A => Int
  type ToSelf[A]            = A => A
  type ToString[-A]         = A => String
  type ToUnit[-A]           = A => Unit
  type UShort               = Char // unsigned short
}

// Necessary to use those aliases within the api package.
object ApiAliases extends ApiAliases

trait ApiMethods extends scala.Any {
  import ApiAliases._

  def ?[A](implicit value: A): A                 = value
  def emptyValue[A](implicit z: Empty[A]): A     = z.empty
  def identity[A](x: A): A                       = x
  def implicitly[A](implicit x: A): A            = x
  def newArray[A: CTag](size: Precise): Array[A] = new Array[A](size.intValue)
  def show[A: Show] : Show[A]                    = ?
  def longCmp(diff: Long): Cmp                   = if (diff < 0) Cmp.LT else if (diff > 0) Cmp.GT else Cmp.EQ
}
object ApiMethods extends ApiMethods
