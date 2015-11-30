package psp
package std

import api._
import Consecutive.empty

final class Consecutive[+A] private (val startInt: Int, val lastInt: Int, f: Int => A) extends Direct[A] with ForceShowDirect {
  private def create(startInt: Int, size: Precise): Consecutive[A] =
    if (isEmpty || size.isZero) empty
    else new Consecutive[A](startInt, startInt + size.getInt - 1, f)

  def isEmpty                           = lastInt < startInt
  def size                              = Size(hops + 1)
  def exclusiveEnd: Int                 = lastInt + 1
  def hops: Int                         = lastInt - startInt
  def elemAt(index: Index): A           = f(startInt + index.getInt)
  def foreach(g: A => Unit): Unit       = if (!isEmpty) lowlevel.ll.foreachConsecutive(startInt, lastInt, f andThen g)
  def containsInt(n: Int): Bool         = startInt <= n && n < exclusiveEnd
  def containsIndex(index: Index)       = size containsIndex index
  def map[B](g: A => B): Consecutive[B] = new Consecutive(startInt, lastInt, f andThen g)
  def asIndices: IndexRange             = startInt to lastInt map (i => Index(i))

  def drop(n: Precise): Consecutive[A]        = create(startInt + n.toInt, size - n)
  def dropRight(n: Precise): Consecutive[A]   = create(startInt, size - n)
  def take(n: Precise): Consecutive[A]        = create(startInt, size min n)
  def takeRight(n: Precise): Consecutive[A]   = (size min n) |> (s => create(exclusiveEnd - s.toInt, s))
  def slice(s: Long, e: Long): Consecutive[A] = if (s < 0) slice(0, e) else if (e <= 0 || e <= s) empty else this drop s take e - s
  def slice(r: IndexRange): Consecutive[A]    = slice(r.startInt, r.exclusiveEnd)
  def tail: Consecutive[A]                    = this drop 1
  def init: Consecutive[A]                    = this dropRight 1

  def << (n: Int): Consecutive[A] = create(startInt - n, size)

  def to_s = if (isEmpty) "[]" else s"[$startInt..$lastInt]"
}

object Consecutive {
  private val id: Int => Int = x => x
  private val Empty = new Consecutive[Nothing](0, -1, indexOutOfBoundsException)

  def empty: Consecutive[Nothing]                                 = Empty
  def downTo(start: Int, end: Int): Direct[Int]                   = to(end, start).reverse
  def to(start: Int, end: Int): IntRange                          = if (end < start) empty else new Consecutive(start, end, id)
  def until(start: Int, end: Int): IntRange                       = if (end <= start) empty else new Consecutive(start, end - 1, id)
  def until[A](start: Int, end: Int, f: Int => A): Consecutive[A] = if (end <= start) empty else new Consecutive(start, end - 1, f)
}
