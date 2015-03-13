package psp
package std

import api._

object ExSet {
  def apply[A: Eq](xs: Each[A]): ExSet[A]                  = new Impl[A](xs, ?)
  def natural[A](xs: Each[A]): ExSet[A]                    = apply[A](xs)(NaturalEq)
  def reference[A <: AnyRef](xs: Each[A]): ExSet[A]        = apply[A](xs)(ReferenceEq)
  def shown[A: Show](xs: Each[A]): ExSet[A]                = apply[A](xs)(Eq.shown[A])
  def direct[A](xs: Each[A])(equiv: Relation[A]): ExSet[A] = apply[A](xs)(Eq(equiv))
  def elems[A: Eq](xs: A*): ExSet[A]                       = apply[A](Direct(xs: _*))

  def fromJava[A](xs: jSet[A]): ExSet[A]   = new FromJava(xs)
  def fromScala[A](xs: scSet[A]): ExSet[A] = new FromScala(xs.toSet)

  def impl[A](xs: ExSet[A]): Impl[A] = xs match {
    case xs: Impl[A] => xs
    case _           => new Impl[A](xs.toEach, Eq(xs.equiv))
  }

  sealed trait ExSetImpl[A] extends ExSet[A] with ToBool[A] {
    def eqs: Eq[A]
    def equiv(x: A, y: A): Boolean = eqs.equiv(x, y)
  }
  class FromScala[A](xs: sciSet[A]) extends ExSetImpl[A] {
    def size: IntSize                 = Precise(xs.size)
    @inline def foreach(f: A => Unit) = xs foreach f
    def apply(elem: A)                = xs(elem)
    def eqs                           = Eq.natural()
  }
  class FromJava[A](xs: jSet[A]) extends ExSetImpl[A] {
    def size: IntSize                 = Precise(xs.size)
    @inline def foreach(f: A => Unit) = xs.iterator foreach f
    def apply(elem: A)                = xs contains elem
    def eqs                           = Eq.natural()
  }

  sealed trait Derived[A] extends ExSetImpl[A] {
    protected def underlying: ExSet[A]
    def eqs = underlying.eqs
  }
  final case class Filtered[A](lhs: ExSet[A], p: ToBool[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && p(elem)
    @inline def foreach(f: A => Unit): Unit = lhs foreach (x => if (p(x)) f(x))
    def size                                = lhs.size.atMost
  }
  final case class Intersect[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && rhs(elem)
    @inline def foreach(f: A => Unit): Unit = lhs filter rhs foreach f
    def size                                = lhs.size intersect rhs.size
  }
  final case class Union[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) || rhs(elem)
    @inline def foreach(f: A => Unit): Unit = Each.join(lhs, rhs filterNot lhs) foreach f
    def size                                = lhs.size union rhs.size
  }
  final case class Diff[A](lhs: ExSet[A], rhs: ExSet[A]) extends Derived[A] {
    protected def underlying                = lhs
    def apply(elem: A)                      = lhs(elem) && !rhs(elem)
    @inline def foreach(f: A => Unit): Unit = lhs filterNot rhs foreach f
    def size                                = lhs.size diff rhs.size
  }
  final class Impl[A](basis: Each[A], val eqs: Eq[A]) extends ExSetImpl[A] {
    private[this] val wrap                     = WrapEqHashShow[A]
    private[this] val wrapSet: jSet[wrap.Wrap] = basis map (x => wrap(x)) toJavaSet
    def apply(elem: A)                         = wrapSet contains wrap(elem)
    def size: Precise                          = wrapSet.size.size
    @inline def foreach(f: A => Unit): Unit    = wrapSet foreach (x => f(x.value))
  }
}

object InSet {
  val Zero = Pure[Any](ConstantFalse)
  val One  = Pure[Any](ConstantTrue)

  def impl[A](xs: InSet[A]): Impl[A] = xs match {
    case xs: Impl[A] => xs
    case _           => apply[A](xs)
  }
  def apply[A](p: ToBool[A]): InSet[A] = p match {
    case ConstantFalse => Zero
    case ConstantTrue  => One
    case _             => Pure[A](p)
  }
  def doc[A](xs: InSet[A]): Doc = xs match {
    case Zero                => "∅".s
    case One                 => "U".s
    case Complement(xs)      => doc"$xs′"
    case Intersect(lhs, rhs) => doc"$lhs ∩ $rhs"
    case Union(lhs, rhs)     => doc"$lhs ∪ $rhs"
    case Diff(lhs, rhs)      => doc"$lhs ∖ $rhs"
    case Pure(f: ShowDirect) => doc"$f" // f.to_s
    case _                   => "{ ... }".s
  }

  abstract class Impl[A](p: ToBool[A])                        extends InSet[A] with ToBool[A] { def apply(x: A) = p(x) ; override def toString = doc(this).render }
  final case class Complement[A](lhs: InSet[A])               extends Impl[A](x => !lhs(x))
  final case class Intersect[A](lhs: InSet[A], rhs: InSet[A]) extends Impl[A](x => lhs(x) && rhs(x))
  final case class Union[A](lhs: InSet[A], rhs: InSet[A])     extends Impl[A](x => lhs(x) || rhs(x))
  final case class Diff[A](lhs: InSet[A], rhs: InSet[A])      extends Impl[A](x => lhs(x) && !rhs(x))
  final case class Pure[A](p: ToBool[A])                      extends Impl[A](p)
}
