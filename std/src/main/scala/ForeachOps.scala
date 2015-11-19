package psp
package std
package ops

import api._
import java.lang.System.arraycopy
import Api.SpecTypes

final class InPlace[A](val xs: Array[A]) extends AnyVal {
  private def andThis(op: Unit): Array[A] = xs
  private def lastIndex = xs.length - 1
  private def sortRef(cmp: Comparator[A]) = java.util.Arrays.sort[A](xs.castTo[Array[Ref[A]]], cmp)
  private def isReference = (xs: Any) match {
    case _: Array[AnyRef] => true
    case _                => false
  }
  private def midpoint: Int  = xs.length / 2
  private def swap(i1: Int, i2: Int) {
    val tmp = xs(i1)
    xs(i1) = xs(i2)
    xs(i2) = tmp
  }

  def map(f: ToSelf[A]): Array[A]           = andThis(0 to lastIndex foreach (i => xs(i) = f(xs(i))))
  def sort(implicit z: Order[A]): Array[A]  = andThis(if (isReference) sortRef(Order.comparator) else Array sortInPlace xs)
  def sortBy[B: Order](f: A => B): Array[A] = sort(orderBy[A](f))
  def reverse(): Array[A]                   = andThis(0 until midpoint foreach (i => swap(i, lastIndex - i)))
  def shuffle(): Array[A]                   = andThis(0 until lastIndex foreach (i => swap(i, i + randomNat(lastIndex - i))))
}

final class ArrayClassTagOps[A: CTag](val xs: Array[A]) {
  def ++(that: Array[A]): Array[A] = {
    val arr = newArray[A](xs.length + that.length)
    arraycopy(xs, 0, arr, 0, xs.length)
    arraycopy(that, 0, arr, xs.length, that.length)
    arr
  }
}

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  def size: Precise                          = Size(xs.length)
  def apply(idx: Index): A                   = xs(idx.getInt)
  def updated(idx: Index, value: A): xs.type = andThis(xs(idx.getInt) = value)
  def mapNow[B: CTag](f: A => B): Array[B]   = {
    val arr = newArray[B](xs.length)
    foreachIntIndex(i => arr(i) = f(xs(i)))
    arr
  }
  def inPlace: InPlace[A]                    = new InPlace[A](xs)
  private def andThis(op: Unit): xs.type     = xs
}

final class ForeachOps[A](val xs: Each[A]) extends AnyVal {
  def sized(size: Precise): Each[A] = new Each.Sized(xs, size)
  def memo: Indexed.Memo[A] = xs match {
    case xs: Indexed.Memo[A] => xs
    case _                   => new Indexed.Memo(xs)
  }
}

final class DirectOps[A](val xs: Direct[A]) extends AnyVal {
  def containsIndex(i: Index): Boolean = 0 <= i.getInt && i.getInt < xs.intSize
  def apply(i: Index): A               = xs elemAt i
  def length: Int                      = xs.intSize
  def mapNow[B](f: A => B): Vec[B]     = xs.indices map (i => f(xs(i))) toVec
}
