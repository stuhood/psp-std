package psp
package api

import scala.Any
import scala.Boolean
import scala.Unit
import scala.Int
import scala.Long
import java.lang.String
import scala.{ Product2 => -> }

/** The classic type classes for encoding value equivalence and hash codes.
 */
trait Eq[-A]     extends Any                         { def equiv(x: A, y: A): Boolean }
trait Hash[-A]   extends Any                         { def hash(x: A): Int            }
trait HashEq[-A] extends Any with Hash[A] with Eq[A]

/** The classic type class for turning string representations into typed values.
 */
trait Read[A] extends Any { def read(x: String): A }

/** The classic type class for turning typed values into string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

/** The original type class for providing the "empty" value of a particular type.
 *  Suitable only for types with a unique (useful) definition of empty - but that's
 *  a whole lot of types.
 */
trait Empty[+A] extends Any { def empty: A }

/** The collections builder type class. Not especially classic in this presentation.
 */
trait Builds[-Elem, +To] extends Any { def build(xs: Each[Elem]): To }

/** Contravariance vs. implicits, the endless battle.
 *  We return a java three-value enum from compare in preference
 *  to a wild stab into the 2^32 states of an Int. This is a
 *  controversial thing to do, in the year 2014. Not joking.
 */
trait Order[-A] extends Any { def compare(x: A, y: A): Cmp }

/** When a 3-value enum isn't enough, try a 4-value one.
 */
trait PartialOrder[-A] extends Any { def partialCompare(x: A, y: A): PCmp }

/** Name-based extractor methods.
 */
trait IsEmpty extends Any                { def isEmpty: Boolean }
trait Opt[+A] extends Any with IsEmpty   { def get: A           }
trait OptInt extends Any with Opt[Int]   { def get: Int         }
trait OptLong extends Any with Opt[Long] { def get: Long        }
trait Index extends Any with OptLong     { def index: Long      }

/** Collections interfaces.
 *  We'd much prefer not to extend Function1 here, but the consequences
 *  for type inference are too severe, and the last thing we want to do is
 *  create disincentives to work in terms of the API types.
 */
trait Each[+A] extends Any with HasSize                          { def foreach(f: A => Unit): Unit   }
trait Indexed[+A] extends Any with Each[A]                       { def elemAt(i: Index): A           }
trait Direct[+A] extends Any with Indexed[A] with HasPreciseSize
trait Linear[+A] extends Any with Each[A] with IsEmpty           { def head: A ; def tail: Linear[A] }

trait Intensional[-K, +V] extends Any                              { def apply(x: K): V       }
trait InSet[-A]           extends Any with Intensional[A, Boolean] { def apply(x: A): Boolean }
trait InMap[-K, +V]       extends Any with Intensional[K, V]       { def domain: InSet[K]     }

trait Extensional[+A]     extends Any with Each[A]
trait ExSet[A]            extends Any with Extensional[A] with InSet[A]         { def hashEq: HashEq[A] }
trait ExMap[K, +V]        extends Any with Extensional[K -> V] with InMap[K, V] { def domain: ExSet[K]  }

/** Generalized type constraint.
 */
sealed abstract class <:<[-From, +To] extends (From => To)
final class conformance[A] extends <:<[A, A] { def apply(x: A): A = x }
