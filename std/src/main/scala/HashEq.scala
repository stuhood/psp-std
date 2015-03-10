package psp
package std

import api._

/** A bad idea in general, but so much less ceremony for limited-use classes.
 */
trait NaturalHashEq

object Hash {
  def reference[A](): Hash[Ref[A]]   = new impl.HashImpl[Ref[A]](_.id_##)
  def natural[A](): Hash[A]          = new impl.HashImpl[A](_.##)
  def apply[A](f: A => Int): Hash[A] = new impl.HashImpl[A](f)
}

object Eq {
  def reference[A](): Eq[Ref[A]]      = new impl.EqImpl[Ref[A]](_ id_== _)
  def natural[A](): Eq[A]             = new impl.EqImpl[A](_ == _)
  def apply[A](f: Relation[A]): Eq[A] = new impl.EqImpl[A](f)
}

trait HashEqLow {
  implicit def universalEq[A <: NaturalHashEq] : HashEq[A] = HashEq.natural()
}

object HashEq extends HashEqLow {
  implicit def composeHashEq[A](implicit eqs: Eq[A], hash: Hash[A]): HashEq[A] = new impl.HashEqImpl[A](eqs.equiv, hash.hash)

  def apply[A](cmp: Relation[A], hashFn: A => Int): HashEq[A] = new impl.HashEqImpl[A](cmp, hashFn)

  def natural[A](eqs: Eq[A]): HashEq[A]        = apply[A](eqs.equiv, _.##)
  def natural[A](): HashEq[A]                  = apply[A](_ == _, _.##)
  def reference[A <: AnyRef](): HashEq[Ref[A]] = apply[Ref[A]](_ eq _, identityHashCode)
  def shown[A: Show](): HashEq[A]              = apply[A](_.to_s == _.to_s, _.to_s.##)

  final case class Wrap[A: HashEq](value: A) {
    override def hashCode = value.hash
    override def equals(x: Any): Boolean = x match {
      case Wrap(that) => value === that.castTo[A]
      case _          => false
    }
    override def toString = pp"$value"
  }
}
