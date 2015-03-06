package psp
package std

import api._

object StdEq extends impl.EqInstances
object StdShow extends ShowInstances

object Unsafe extends LowPriorityUnsafe {
  implicit def universalEq[A] : HashEq[A]        = HashEq.natural()
  implicit def universalShow[A] : Show[A]        = Show.natural()
  implicit def showableOrder[A: Show] : Order[A] = orderBy[A](_.to_s)
}
trait LowPriorityUnsafe {
  // We may as well derive some convenience from the absence of parametricity.
  implicit def universalOrder[A] : Order[A] = orderBy[A]("" + _) | (_.##)
}
