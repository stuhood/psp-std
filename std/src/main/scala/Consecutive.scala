package psp
package std

import api._, StdEq._

/** A class for anything derived from a consecutive sequence of Ints.
 *  It saves a lot of code to let int ranges be an instance of this with the identity function.
 *  We can revisit on performance, Longs, etc.
 */
sealed class Consecutive[+A] private[std] (val startInt: Int, val lastInt: Int, f: Int => A) extends Direct[A] {
  import Consecutive.empty
  private def hops = if (isEmpty) -1 else lastInt - startInt
  private def create(startInt: Int, size: Precise): Consecutive[A] =
    if (size.isZero) empty
    else new Consecutive[A](startInt, size.toInt + startInt - 1, f)

  def endInt: Int         = lastInt + 1
  def size                = Size(hops + 1)
  def isEmpty: Boolean    = lastInt < startInt
  def elemAt(i: Index): A = f(startInt + i.getInt)
  def map[B](g: A => B)   = new Consecutive[B](startInt, lastInt, f andThen g)

  def containsInt(n: Int): Bool               = startInt <= n && n <= lastInt
  def foreach(g: A => Unit): Unit             = if (!isEmpty) lowlevel.ll.foreachConsecutive(startInt, lastInt, f andThen g)
  def asIndices: IndexRange                   = Consecutive.to(startInt, lastInt) map (i => Index(i))
  def tail: Consecutive[A]                    = drop(1)
  def init: Consecutive[A]                    = dropRight(1)
  def drop(n: Precise): Consecutive[A]        = create(startInt + n.toInt, size - n)
  def dropRight(n: Precise): Consecutive[A]   = create(startInt, size - n)
  def take(n: Precise): Consecutive[A]        = create(startInt, min(size, n))
  def takeRight(n: Precise): Consecutive[A]   = min(size, n) |> (s => create(endInt - s.toInt, s))
  def slice(s: Int, e: Int): Consecutive[A]   = if (e <= 0 || e <= s) empty else this drop s take e - s
  def slice(r: IndexRange): Consecutive[A]    = slice(r.startInt, r.endInt)
  def dropWhile(p: ToBool[A]): Consecutive[A] = prefixLength(p) |> (len => create(startInt + len, size - len))
  def takeWhile(p: ToBool[A]): Consecutive[A] = create(startInt, Size(prefixLength(p)))
  def >> (n: Int): Consecutive[A]             = create(startInt + n, size)
  def << (n: Int): Consecutive[A]             = create(startInt - n, size)

  def toDrop: Precise = Size(startInt)
  def toTake: Precise = size

  private def prefixLength(p: ToBool[A]): Int = {
    val max = size.toInt
    @tailrec def loop(count: Int): Int = (
      if (count >= max) max
      else if (p(f(startInt + count))) loop(count + 1)
      else count
    )
    loop(0)
  }

  override def toString = if (isEmpty) "[]" else s"[$startInt..$lastInt]"
}

object Consecutive {
  private val id: Int => Int = x => x
  final class Ints private[std] (s: Int, e: Int) extends Consecutive[Int](s, e, id)
  val empty = new Consecutive[Nothing](0, -1, _ => abort("empty"))

  def downTo(start: Int, end: Int): Direct[Int]                   = Direct reversed to(end, start)
  def to(start: Int, end: Int): Consecutive[Int]                  = if (end < start) empty else new Ints(start, end)
  def until(start: Int, end: Int): Consecutive[Int]               = if (end <= start) empty else new Ints(start, end - 1)
  def until[A](start: Int, end: Int, f: Int => A): Consecutive[A] = if (end <= start) empty else new Consecutive(start, end - 1, f)
}
