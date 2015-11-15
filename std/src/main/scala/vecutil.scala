package psp
package std

import scala.compat.Platform.arraycopy

/** Taking a stab at deciphering the scala Vector implementation
 *  in the interests of specializing for primitive types.
 */
final class Base32(val index: Int) extends AnyVal {
  def digitAt(place: Int): Int = apply(place)
  def apply(place: Int): Int = place match {
    case 0 => index & 31
    case 1 => (index >>  5) & 31
    case 2 => (index >> 10) & 31
    case 3 => (index >> 15) & 31
    case 4 => (index >> 20) & 31
    case 5 => (index >> 25) & 31
  }
  def level = (
    if (index < 32) 0
    else if ((index >> 5) < 32) 1
    else if ((index >> 10) < 32) 2
    else if ((index >> 15) < 32) 3
    else if ((index >> 20) < 32) 4
    else if ((index >> 25) < 32) 5
    else 6
  )
  def places = 0 to level
  def digits = (places map digitAt).toVec.reverseIterator.toVec

  private def digit32(n: Int): String = (if (n < 10) '0' + n else 'A' + n - 10).toChar.toString
  override def toString = "[B32:%s]".format(digits map digit32 mkString "")
}

final class Displays {
  final val Width    = 6
  final val displays = new Array[Array[AnyRef]](Width)
  var depth: Int = _

  @inline private implicit def fixArray(x: AnyRef): Array[AnyRef] = x.asInstanceOf[Array[AnyRef]]

  def levels = (1 to depth).toVec.reverseIterator

  private def copyOf(a: Array[AnyRef]): Array[AnyRef] =
    new Array[AnyRef](a.length) doto (b => arraycopy(a, 0, b, 0, a.length))

  def stabilize(idx: Base32): Unit = {
    levels foreach (n => displays(n) = copyOf(displays(n)))
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
