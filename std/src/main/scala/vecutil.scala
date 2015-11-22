package psp
package std

import Vec._
import StdShow._

sealed trait ArrayN[@spec A] extends AnyRef {
  type Elem
  type This = ArrayN.Typed[A, Elem]

  def xs: Array[Elem]
  def apply(n: Int): A

  def length                                        = xs.length
  def update(index: Int, value: Elem): Unit         = xs(index) = value
  def updateUnchecked(index: Int, value: Any): Unit = update(index, value.asInstanceOf[Elem])
}
final case class Array0[@spec A](xs: Array[A]) extends ArrayN[A] {
  type Elem = A
  def apply(n: Int): A = xs(n & MASK5)
}
final case class Array1[@spec A](xs: Array2D[A]) extends ArrayN[A] {
  type Elem = Array[A]
  def apply(n: Int): A = xs(n >>> 5 & MASK5)(n & MASK5)
}
final case class Array2[@spec A](xs: Array3D[A]) extends ArrayN[A] {
  type Elem = Array2D[A]
  def apply(n: Int): A = xs(n >>> 10 & MASK5)(n >>> 5 & MASK5)(n & MASK5)
}
final case class Array3[@spec A](xs: Array4D[A]) extends ArrayN[A] {
  type Elem = Array3D[A]
  def apply(n: Int): A = xs(n >>> 15 & MASK5)(n >>> 10 & MASK5)(n >>> 5 & MASK5)(n & MASK5)
}
final case class Array4[@spec A](xs: Array5D[A]) extends ArrayN[A] {
  type Elem = Array4D[A]
  def apply(n: Int): A = xs(n >>> 20 & MASK5)(n >>> 15 & MASK5)(n >>> 10 & MASK5)(n >>> 5 & MASK5)(n & MASK5)
}
final case class Array5[@spec A](xs: Array6D[A]) extends ArrayN[A] {
  type Elem = Array5D[A]
  def apply(n: Int): A = xs(n >>> 25 & MASK5)(n >>> 20 & MASK5)(n >>> 15 & MASK5)(n >>> 10 & MASK5)(n >>> 5 & MASK5)(n & MASK5)
}

object ArrayN {
  type Typed[A, E] = ArrayN[A] { type Elem = E }
  def apply[@spec A : CTag](len: Int): Array0[A] = new Array0[A](newArray[A](len))
}

class ArrayLevels[@spec A: CTag] {
  var depth: Int = _

  final val Width   = 6
  final val displays = new Array[ArrayN[A]](Width)

  def getDisplay(n: Int): ArrayN[A]           = displays(n)
  def setDisplay(n: Int, xs: ArrayN[A]): Unit = displays(n) = xs

  private def copyOf(a: ArrayN[A]): ArrayN[A] = a match {
    case Array0(xs) => Array0(copyArray(xs))
    case Array1(xs) => Array1(copyArray(xs))
    case Array2(xs) => Array2(copyArray(xs))
    case Array3(xs) => Array3(copyArray(xs))
    case Array4(xs) => Array4(copyArray(xs))
    case Array5(xs) => Array5(copyArray(xs))
  }

  def stabilize(index: Int): Unit = {
    val idx = new Base32(index)
    def levels = depth - 1 downTo 1
    levels foreach (n => displays(n) = copyOf(displays(n)))
    levels foreach (n => displays(n).updateUnchecked(idx digitAt n, displays(n - 1)))
  }

  private[std] final def copyRange[E: CTag](array: ArrayN.Typed[A, E], oldLeft: Int, newLeft: Int): Array[E] = {
    val elems = new Array[E](32)
    arraycopy[E](array.xs, oldLeft, elems, newLeft, 32 - max(newLeft, oldLeft))
    elems
  }

  private[std] def shiftTopLevel(oldLeft: Int, newLeft: Int) = displays(depth - 1) match {
    case xs: Array0[A] => copyRange(xs, oldLeft, newLeft)
    case xs: Array1[A] => copyRange(xs, oldLeft, newLeft)
    case xs: Array2[A] => copyRange(xs, oldLeft, newLeft)
    case xs: Array3[A] => copyRange(xs, oldLeft, newLeft)
    case xs: Array4[A] => copyRange(xs, oldLeft, newLeft)
    case xs: Array5[A] => copyRange(xs, oldLeft, newLeft)
  }

  def initFrom(parent: ArrayLevels[A]): Unit = {
    depth = parent.depth
    0 until depth foreach (n => displays(n) = parent.displays(n))
  }

  def getElem(n: Int, level: Int): A = displays(level) match {
    case xs: Array0[A] => xs(n)
    case xs: Array1[A] => xs(n)
    case xs: Array2[A] => xs(n)
    case xs: Array3[A] => xs(n)
    case xs: Array4[A] => xs(n)
    case xs: Array5[A] => xs(n)
    case n             => illegalArgumentException(n)
  }
}

/** Taking a stab at deciphering the scala Vector implementation
 *  in the interests of specializing for primitive types.
 */
final class Base32(val index: Int) extends AnyVal {
  def digitAt(place: Int): Int = apply(place)

  def apply(place: Int): Int = index >>> place * 5 & 31

  def level  = levelOf(index)
  def places = 0 to level
  def digits = (places map digitAt).toVec.reverseIterator.toVec

  private def digit32(n: Int): String = (if (n < 10) '0' + n else 'A' + n - 10).toChar.toString
  override def toString = "[B32:%s]".format(digits map digit32 join_s)
}

final class Displays {
  final val Width    = 6
  final val displays = new Array[Array[AnyRef]](Width)
  var depth: Int = _

  @inline private implicit def fixArray(x: AnyRef): Array[AnyRef] = x.asInstanceOf[Array[AnyRef]]

  def levels = (1 to depth).toVec.reverseIterator

  def stabilize(idx: Base32): Unit = {
    levels foreach (n => displays(n) = copyArray(displays(n)))
    levels foreach (n => displays(n)(idx digitAt n) = displays(n - 1))
  }

  def initFrom(parent: Displays): Unit = {
    depth = parent.depth
    0 until depth foreach (n => displays(n) = parent.displays(n))
  }

  def get(arr: Array[AnyRef], d0: Int): AnyRef                                              = arr(d0)
  def get(arr: Array[AnyRef], d1: Int, d0: Int): AnyRef                                     = get(arr(d1), d0)
  def get(arr: Array[AnyRef], d2: Int, d1: Int, d0: Int): AnyRef                            = get(arr(d2), d1, d0)
  def get(arr: Array[AnyRef], d3: Int, d2: Int, d1: Int, d0: Int): AnyRef                   = get(arr(d3), d2, d1, d0)
  def get(arr: Array[AnyRef], d4: Int, d3: Int, d2: Int, d1: Int, d0: Int): AnyRef          = get(arr(d4), d3, d2, d1, d0)
  def get(arr: Array[AnyRef], d5: Int, d4: Int, d3: Int, d2: Int, d1: Int, d0: Int): AnyRef = get(arr(d5), d4, d3, d2, d1, d0)

  def getElem(n: Base32): AnyRef = n.level match {
    case 0 => get(displays(0), n(0))
    case 1 => get(displays(1), n(1), n(0))
    case 2 => get(displays(2), n(2), n(1), n(0))
    case 3 => get(displays(3), n(3), n(2), n(1), n(0))
    case 4 => get(displays(4), n(4), n(3), n(2), n(1), n(0))
    case 5 => get(displays(5), n(5), n(4), n(3), n(2), n(1), n(0))
    case n => illegalArgumentException(n)
  }

  def getDisplay(n: Int): Array[AnyRef]           = displays(n)
  def setDisplay(n: Int, xs: Array[AnyRef]): Unit = displays(n) = xs
}
