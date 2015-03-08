package psp
package std
package ops

import api._

final class ArrayInPlaceOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  private def andThis(op: Unit): Array[A]  = xs
  def size: IntSize                        = Precise(xs.length)
  def map(f: ToSelf[A]): Array[A]          = andThis(foreachIntIndex(i => xs(i) = f(xs(i))))
  def sort(implicit z: Order[A]): Array[A] = andThis(
    (xs: Array[_]) match {
      case _: Array[AnyRef] => java.util.Arrays.sort[A](xs.castTo[Array[A with AnyRef]], z.toComparator)
      case _                => Array sortInPlace xs
    }
  )
  def reverse(): Array[A] = andThis(
    0 until xs.length / 2 foreach { i =>
      val j = xs.length - 1 - i
      val tmp = xs(j)
      xs(j) = xs(i)
      xs(i) = tmp
    }
  )
}

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  def size: IntSize                          = Precise(xs.length)
  def apply(idx: Index): A                   = xs(idx.getInt)
  def updated(idx: Index, value: A): xs.type = andThis(xs(idx.getInt) = value)
  def mapNow[B: CTag](f: A => B): Array[B]   = newArray[B](size) doto (arr => foreachIntIndex(i => arr(i) = f(xs(i))))
  def inPlace: ArrayInPlaceOps[A]            = new ArrayInPlaceOps[A](xs)
  private def andThis(op: Unit): xs.type = xs
}

final class ForeachOps[A](val xs: Each[A]) extends AnyVal {
  def sized(size: Precise): Each[A] = new Each.Sized(xs, size)
  def memo: Indexed.Memo[A] = xs match {
    case xs: Indexed.Memo[A] => xs
    case _                   => new Indexed.Memo(xs)
  }
}

final class DirectOps[A](val xs: Direct[A]) extends AnyVal {
  def +:(x: A): Direct[A]          = Direct.prepend(x, xs)
  def :+(x: A): Direct[A]          = Direct.append(xs, x)
  def ++(ys: Direct[A]): Direct[A] = Direct.join(xs, ys)

  def apply(i: Index): A           = xs elemAt i
  def length: Int                  = xs.size.intSize
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
  def mapNow[B](f: A => B): Direct[B] = {
    val arr = newArray[Any](xs.size)
    xs.indices foreach (i => arr(i.getInt) = f(xs(i)))
    Direct.wrapArray[B](arr)
  }
}

final class LinearOps[A](val xs: Linear[A]) extends AnyVal {
  def ::(x: A): Linear[A] = Linear.cons(x, xs)
}
