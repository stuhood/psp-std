package psp
package std
package ops

import api._

final class JavaIteratorOps[A](it: jIterator[A]) {
  def foreach(f: A => Unit): Unit = while (it.hasNext) f(it.next)
}

final class CmpEnumOps(val cmp: Cmp) {
  def |(that: => Cmp): Cmp = if (cmp == Cmp.EQ) that else cmp
}
