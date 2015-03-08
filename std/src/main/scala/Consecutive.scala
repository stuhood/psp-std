package psp
package std

import api._

/** A class for anything derived from a consecutive sequence of Ints.
 *  It saves a lot of code to let int ranges be an instance of this with the identity function.
 *  We can revisit on performance, Longs, etc.
 */
sealed class Consecutive[+A] private[std] (val startInt: Int, val lastInt: Int, f: Int => A) extends Direct[A] {
  import Consecutive.empty
  private def hops = if (isEmpty) -1 else lastInt - startInt
  private def create(startInt: Int, size: Precise): Consecutive[A] =
    if (size.isZero) empty
    else new Consecutive[A](startInt, startInt + size.getInt - 1, f)

  def endInt: Int         = lastInt + 1
  def size: Precise       = Precise(hops + 1)
  def isEmpty: Boolean    = lastInt < startInt
  def elemAt(i: Index): A = f(startInt + i.getInt)
  def map[B](g: A => B)   = new Consecutive[B](startInt, lastInt, f andThen g)

  def foreach(g: A => Unit): Unit        = if (!isEmpty) lowlevel.foreachConsecutive(startInt, lastInt, f andThen g)
  def foreachReverse(g: A => Unit): Unit = if (!isEmpty) {
    val h = f andThen g
    lowlevel.foreachConsecutive(0, hops, i => h(lastInt - i))
  }

  def asIndices: IndexRange                   = Consecutive.to(startInt, lastInt) map (i => Index(i))
  def tail: Consecutive[A]                    = drop(1.size)
  def init: Consecutive[A]                    = dropRight(1.size)
  def drop(n: Precise): Consecutive[A]        = create(startInt + n.getInt, size - n)
  def dropRight(n: Precise): Consecutive[A]   = create(startInt, size - n)
  def take(n: Precise): Consecutive[A]        = create(startInt, size min n)
  def takeRight(n: Precise): Consecutive[A]   = (size min n) |> (s => create(endInt - s.getInt, s))
  def slice(s: Int, e: Int): Consecutive[A]   = if (e <= 0 || e <= s) empty else this drop s.size take (e - s).size
  def slice(r: IndexRange): Consecutive[A]    = slice(r.startInt, r.endInt)
  def dropWhile(p: ToBool[A]): Consecutive[A] = prefixLength(p) |> (len => create(startInt + len, size - len))
  def takeWhile(p: ToBool[A]): Consecutive[A] = create(startInt, Precise(prefixLength(p)))
  def >> (n: Int): Consecutive[A]             = create(startInt + n, size)
  def << (n: Int): Consecutive[A]             = create(startInt - n, size)

  def toDrop: Precise = Precise(startInt)
  def toTake: Precise = size
  def prefixLength(p: ToBool[A]): Int = {
    var count = 0
    while (count < size.getInt) {
      if (!p(f(startInt + count))) return count
      count += 1
    }
    count
  }

  override def toString = if (isEmpty) "[]" else s"[$startInt..$lastInt]"
}

object Consecutive {
  private val id: Int => Int = x => x
  final class Ints private[std] (s: Int, e: Int) extends Consecutive[Int](s, e, id)
  val empty = new Consecutive[Nothing](0, -1, _ => sys.error("empty"))

  def to(start: Int, end: Int): Consecutive[Int]                  = if (end < start) empty else new Ints(start, end)
  def until(start: Int, end: Int): Consecutive[Int]               = if (end <= start) empty else new Ints(start, end - 1)
  def until[A](start: Int, end: Int, f: Int => A): Consecutive[A] = if (end <= start) empty else new Consecutive(start, end - 1, f)
}
