package psp
package std

import impl.Size._, api._
import lowlevel.CircularBuffer

sealed abstract class AtomicView[A, Repr] extends InvariantBaseView[A, Repr] {
  type This <: AtomicView[A, Repr]
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange
}

object FlattenSlice {
  def unapply[A, Repr](xs: BaseView[A, Repr]): Option[(BaseView[A, Repr], IndexRange)] = xs match {
    case xs: DirectView[_, _]     => Some(xs -> xs.indices)
    case LabeledView(xs, _)       => unapply(xs)
    case Mapped(xs, f)            => unapply(xs) map { case (xs, range) => (xs map f, range) }
    case Dropped(xs, Precise(0))  => unapply(xs)
    case DroppedR(xs, Precise(0)) => unapply(xs)
    case Taken(xs, Precise(0))    => Some(emptyValue)
    case TakenR(xs, Precise(0))   => Some(emptyValue)
    case DroppedR(xs, n)          => unapply(xs) map { case (xs, range) => (xs, range dropRight n) }
    case TakenR(xs, n)            => unapply(xs) map { case (xs, range) => (xs, range takeRight n) }
    case Dropped(xs, n)           => unapply(xs) map { case (xs, range) => (xs, range drop n) }
    case Taken(xs, n)             => unapply(xs) map { case (xs, range) => (xs, range take n) }
    case HasSize(n: Precise)      => Some(xs -> n.indices)
    case _                        => None
  }
}

final class LinearView[A, Repr](underlying: Each[A]) extends AtomicView[A, Repr] {
  type This   = LinearView[A, Repr]
  def viewOps = vec("<list>".s)
  def size    = underlying.size

  @inline def foreach(f: A => Unit): Unit                       = linearlySlice(underlying, fullIndexRange, f)
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = linearlySlice(underlying, range, f)
}

final class IndexedView[A, Repr](val underlying: Indexed[A]) extends AtomicView[A, Repr] with Indexed[A] {
  type This = IndexedView[A, Repr]

  def viewOps                                                   = vec("<indexed>".s)
  def size: Size                                                = underlying.size
  def elemAt(i: Index): A                                       = underlying elemAt i
  def foreach(f: A => Unit): Unit                               = underlying foreach f
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = ??? // underlying slice range foreach f
}

final class DirectView[A, Repr](underlying: Direct[A]) extends AtomicView[A, Repr] with Direct[A] with ops.HasPreciseSizeMethods {
  type This = DirectView[A, Repr]

  def viewOps                                                   = vec("<vector>".s)
  def size: Precise                                             = underlying.size
  def elemAt(i: Index): A                                       = underlying(i)
  def foreach(f: A => Unit): Unit                               = directlySlice(underlying, size.indices, f)
  def foreachSlice(range: IndexRange)(f: A => Unit): IndexRange = directlySlice(underlying, range, f)
}

final case class LabeledView[A, Repr](prev: BaseView[A, Repr], val viewOps: Vec[Doc]) extends BaseView[A, Repr] {
  type This = LabeledView[A, Repr]
  def foreach(f: A => Unit): Unit = prev foreach f
  def description                 = viewOps.last
  def size                        = prev.size
}

sealed trait BaseView[+A, Repr] extends AnyRef with View[A] with ops.ApiViewOps[A] {
  def foreach(f: A => Unit): Unit
  def toEach: Each[A] = Each(foreach)

  type This <: BaseView[A, Repr]
  type MapTo[+X]      = BaseView[X, Repr]
  type Contiguous[+X] = MapTo[X]

  def xs: this.type = this

  def |:(label: String): MapTo[A] = new LabeledView(this, viewOps.init :+ label.s)
  def :|(label: String): MapTo[A] = new LabeledView(this, viewOps.init :+ label.s)

  final def collect[B](pf: A ?=> B): MapTo[B]     = Collected(this, pf)
  final def drop(n: Precise): MapTo[A]            = Dropped(this, n)
  final def dropRight(n: Precise): MapTo[A]       = DroppedR(this, n)
  final def dropWhile(p: ToBool[A]): MapTo[A]     = DropWhile(this, p)
  final def flatMap[B](f: A => Each[B]): MapTo[B] = FlatMapped(this, f)
  final def map[B](f: A => B): MapTo[B]           = Mapped(this, f)
  final def take(n: Precise): MapTo[A]            = Taken(this, n)
  final def takeRight(n: Precise): MapTo[A]       = TakenR(this, n)
  final def takeWhile(p: ToBool[A]): MapTo[A]     = TakenWhile(this, p)
  final def withFilter(p: ToBool[A]): MapTo[A]    = Filtered(this, p)

  final def force[That](implicit z: Builds[A, That]): That = z build this
  final def build(implicit z: Builds[A, Repr]): Repr       = force[Repr]

  def linearlySlice[A](xs: Each[A], range: IndexRange, f: A => Unit): IndexRange = {
    var dropping  = range.startInt
    var remaining = range.intSize
    def nextRange = indexRange(dropping, dropping + remaining)
    xs foreach { x =>
      if (dropping > 0)
        dropping = dropping - 1
      else if (remaining > 0)
        try f(x) finally remaining -= 1

      if (remaining == 0)
        return nextRange
    }
    nextRange
  }
  def directlySlice[A](xs: Direct[A], range: IndexRange, f: A => Unit): IndexRange = {
    xs.indices slice range foreach (i => f(xs(i)))
    range << xs.intSize
  }
}

sealed trait InvariantBaseView[A, Repr] extends BaseView[A, Repr] with InvariantView[A] {
  final def join(that: InvariantView[A]): InvariantView[A] = Joined(this, that)
  final def splitAt(index: Index): Split[A]                = Split(take(index.sizeExcluding), drop(index.sizeExcluding))
  final def span(p: ToBool[A]): Split[A]                   = Split(takeWhile(p), dropWhile(p))
  final def partition(p: ToBool[A]): Split[A]              = Split(withFilter(p), withFilter(!p))
}

sealed abstract class CompositeView[A, B, Repr](val description: Doc, val sizeEffect: ToSelf[Size]) extends InvariantBaseView[B, Repr] {
  def prev: View[A]
  def size    = sizeEffect(prev.size)
  def viewOps = prev.viewOps.castTo[Vec[Doc]] :+ description

  final def foreach(f: B => Unit): Unit = {
    def loop[C](xs: View[C])(f: C => Unit): Unit = {
      type Pred = (ToBool[C] @unchecked) // silencing patmat warnings
      xs match {
        case FlattenSlice(xs, range)  => foreachSlice(xs, range, f)
        case LabeledView(xs, _)       => loop[C](xs)(f)
        case Mapped(xs, g)            => loop(xs)(g andThen f)
        case FlatMapped(xs, g)        => loop(xs)(x => g(x) foreach f)
        case Filtered(xs, p: Pred)    => loop(xs)(x => if (p(x)) f(x))
        case TakenWhile(xs, p: Pred)  => foreachTakeWhile(xs, f, p)
        case DropWhile(xs, p: Pred)   => foreachDropWhile(xs, f, p)
        case Collected(xs, pf)        => loop(xs)(x => if (pf isDefinedAt x) f(pf(x)))
        case Joined(xs, ys)           => loop(xs)(f) ; loop(ys)(f)
        case DroppedR(xs, n: Precise) => foreachDropRight(xs, f, n)
        case TakenR(xs, n: Precise)   => foreachTakeRight(xs, f, n)
        case Dropped(xs, Precise(n))  => foreachSlice(xs, Index(n) until MaxIndex, f)
        case Taken(xs, n: Precise)    => foreachSlice(xs, n.indices, f)
        case xs: View[_]              => xs foreach f
        case _                        => abort(pp"Unexpected view class ${xs.shortClass}")
      }
    }
    if (!size.isZero)
      loop(this)(f)
  }

  private def foreachTakeWhile[A](xs: Each[A], f: A => Unit, p: ToBool[A]): Int = {
    var taken = 0
    xs foreach { x =>
      if (p(x)) f(x) sideEffect (taken += 1)
      else return taken
    }
    taken
  }
  private def foreachDropWhile[A](xs: Each[A], f: A => Unit, p: ToBool[A]): Int = {
    var dropping = true
    var dropped  = 0
    xs foreach { x =>
      if (dropping && p(x)) dropped += 1
      else {
        if (dropping) dropping = false
        f(x)
      }
    }
    dropped
  }

  private def foreachTakeRight[A](xs: Each[A], f: A => Unit, n: Precise): Unit = (
    if (n.isPositive)
      (CircularBuffer[A](n) ++= xs) foreach f
  )
  private def foreachDropRight[A](xs: Each[A], f: A => Unit, n: Precise): Unit = (
    if (n.isZero)
      xs foreach f
    else
      xs.foldl(CircularBuffer[A](n))((buf, x) => if (buf.isFull) try buf finally f(buf push x) else buf += x)
  )

  private def foreachSlice[A](xs: View[A], range: IndexRange, f: A => Unit): Unit = xs match {
    case xs: AtomicView[_, _]                                       => xs.foreachSlice(range)(f)
    case Mapped(prev, g)                                            => foreachSlice(prev, range, g andThen f)
    case xs: Direct[A @unchecked]                                   => directlySlice(xs, range, f)
    case Joined(HasSize(PreciseInt(n)), ys) if n < range.startInt   => ys slice (range << n) foreach f
    case Joined(ys @ HasSize(PreciseInt(n)), _) if range.endInt < n => ys slice range foreach f
    case Joined(ys1, ys2)                                           => linearlySlice(ys1, range, f) |> (remainingRange => linearlySlice(ys2, remainingRange, f))
    case _                                                          => linearlySlice(xs, range, f)
  }
}

final case class Joined [A, B >: A, Repr](prev: BaseView[A, Repr], ys: View[B])     extends CompositeView[A, B, Repr](doc"++ $ys",      _ + ys.size)
final case class Filtered    [A   , Repr](prev: BaseView[A, Repr], p: ToBool[A])    extends CompositeView[A, A, Repr](doc"filter $p",   _.atMost)
final case class Dropped     [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](doc"drop $n",     _ - n)
final case class DroppedR    [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](doc"dropR $n",    _ - n)
final case class Taken       [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](doc"take $n",     _ min n)
final case class TakenR      [A   , Repr](prev: BaseView[A, Repr], n: Precise)      extends CompositeView[A, A, Repr](doc"takeR $n",    _ min n)
final case class TakenWhile  [A   , Repr](prev: BaseView[A, Repr], p: ToBool[A])    extends CompositeView[A, A, Repr](doc"takeW $p",    _.atMost)
final case class DropWhile   [A   , Repr](prev: BaseView[A, Repr], p: ToBool[A])    extends CompositeView[A, A, Repr](doc"dropW $p",    _.atMost)
final case class Mapped      [A, B, Repr](prev: BaseView[A, Repr], f: A => B)       extends CompositeView[A, B, Repr](doc"map $f",      x => x)
final case class FlatMapped  [A, B, Repr](prev: BaseView[A, Repr], f: A => Each[B]) extends CompositeView[A, B, Repr](doc"flatMap $f",  x => if (x.isZero) x else Unknown)
final case class Collected   [A, B, Repr](prev: BaseView[A, Repr], pf: A ?=> B)     extends CompositeView[A, B, Repr](doc"collect $pf", _.atMost)
