package psp
package api

import scala._
import java.lang.String

/** Type classes I'm less certain about keeping.
 */

trait Zero[A] extends Any       { def zero: A ; def isZero(x: A): Boolean }
trait Empty[+A] extends Any     { def empty: A                            }
trait Hash[-A] extends Any      { def hash(x: A): Int                     }
trait Sums[A] extends Any       { def zero: A ; def sum(x: A, y: A): A    }
trait Products[A] extends Any   { def one: A ; def product(x: A, y: A): A }

trait HashEq[-A] extends Any with Hash[A] with Eq[A]

/** The classic type class for encoding string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }

trait Recover[A] extends Any { def onFailure[A](t: Throwable): A }
