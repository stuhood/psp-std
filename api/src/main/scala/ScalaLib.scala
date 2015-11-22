package psp
package ext

import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

/** Building a default namespace consciously rather than accretively.
 *  Primarily these are various "magic" types which cannot be avoided
 *  due to language privilege. An incomplete list:
 *
 *   - all top and bottom types are treated specially
 *   - as are all primitive types and unit
 *   - Seq, Option, and TupleN are hardcoded into extractors
 *   - Seq is hardcoded into varargs
 *   - Arrays are everywhere special
 *   - Lists are specially optimized by the compiler
 *   - StringContext is required for string interpolation
 *   - BigDecimal/BigInt are treated specially for equality
 *   - Dynamic has special semantics
 */
trait ScalaLib {
  // The top and bottom types.
  type Any     = scala.Any
  type AnyRef  = scala.AnyRef
  type AnyVal  = scala.AnyVal
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

  // scala types which it seems counterproductive to rename.
  type Array[A]      = scala.Array[A]
  type BigDecimal    = scala.math.BigDecimal
  type BigInt        = scala.math.BigInt
  type CTag[A]       = scala.reflect.ClassTag[A]
  type Dynamic       = scala.Dynamic
  type Failure[+A]   = scala.util.Failure[A]
  type Option[+A]    = scala.Option[A]
  type Some[+A]      = scala.Some[A]
  type StringContext = scala.StringContext
  type Success[+A]   = scala.util.Success[A]
  type Try[+A]       = scala.util.Try[A]

  // scala annotations
  type inline    = scala.inline
  type spec      = scala.specialized
  type switch    = scala.annotation.switch
  type tailrec   = scala.annotation.tailrec
  type transient = scala.transient
  type uV        = scala.annotation.unchecked.uncheckedVariance
  type unchecked = scala.unchecked
  type volatile  = scala.volatile

  // scala collection types, named consistently and distinctly based on package of origin.
  type scIterable[+A]         = sc.Iterable[A]
  type scIterator[+A]         = sc.Iterator[A]
  type scMap[K, +V]           = sc.Map[K, V]
  type scSeq[+A]              = sc.Seq[A]
  type scSet[A]               = sc.Set[A]
  type scTraversable[+A]      = sc.Traversable[A]
  type sciIndexedSeq[+A]      = sci.IndexedSeq[A]
  type sciList[+A]            = sci.List[A]
  type sciMap[K, +V]          = sci.Map[K, V]
  type sciSeq[+A]             = sci.Seq[A]
  type sciSet[A]              = sci.Set[A]
  type sciStream[+A]          = sci.Stream[A]
  type sciVector[+A]          = sci.Vector[A]
  type scmBuilder[-Elem, +To] = scm.Builder[Elem, To]
  type scmMap[K, V]           = scm.Map[K, V]

  // You can't use string interpolation without a StringContext term in scope.
  def StringContext = scala.StringContext
}
