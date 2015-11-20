package psp
package std

import api._

// TODO - Tremendous sameness to these classes and other potential ones, but
// abstracting over value classes without losing the value-class parts is
// nearly impossible from inside the language. Most likely we have to pursue
// code generation.

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  In principle we could double the usable range by treating
 *  value as an unsigned Int other than -1.
 *
 *  Manipulations of undefined values remain undefined, like NaN.
 */

object Index extends (Long => Index) {
  def undefined: Index             = new Impl(-1)
  def zero: Index                  = new Impl(0)
  def apply(value: Long): Index    = if (value < 0) undefined else new Impl(value)
  def unapply(x: api.Index): Index = x
  def impl(x: api.Index): Impl     = new Impl(x.get)

  final class Impl private[std] (val get: Long) extends AnyVal with api.Index with ForceShowDirect {
    def getInt: Int                   = get.safeInt
    def /(size: Int): Index           = this / Size(size)
    def %(size: Int): Index           = this % Size(size)
    def /(size: Precise): Index       = if (isUndefined) this else Index(get / size.get)
    def %(size: Precise): Index       = if (isUndefined) this else Index(get % size.get)
    def +(n: Long): Index             = if (isUndefined) this else Index(get + n)
    def -(n: Long): Index             = if (isUndefined) this else Index(get - n)
    def prev: Index                   = this - 1
    def next: Index                   = this + 1
    def until(end: Index): IndexRange = indexRange(getInt, end.getInt)
    def sizeExcluding: Precise        = Size(get)
    def sizeIncluding: Precise        = Size(get + 1)
    def toIndex: Index                = this
    def toNth: Nth                    = Nth(get + 1)
    def toOffset: Offset              = if (isUndefined) abort("undefined") else Offset(getInt)
    def isUndefined                   = get < 0
    def isEmpty                       = isUndefined
    def to_s                          = if (isUndefined) "undefined" else s"$get"
  }
}

/** Nth is a 1-based index. The recorded indexValue is 0-based as with Index.
 */
final class Nth private[std] (val get: Long) extends AnyVal with ForceShowDirect {
  def -(n: Long): Nth  = if (isUndefined) this else Nth(get - n)
  def +(n: Long): Nth  = if (isUndefined) this else Nth(get + n)
  def toIndex: Index   = Index(get - 1)
  def toNth: Nth       = this
  def toOffset: Offset = toIndex.toOffset
  def getInt: Int      = get.safeInt
  def isUndefined      = get <= 0
  def to_s             = if (isUndefined) "undefined" else s"#$get"
}

/** Unlike an Index, an Offset can have any integer value.
 *  A negative offset is a positive offset from the other reference point.
 */
final class Offset private[std] (val get: Int) extends AnyVal with ForceShowDirect {
  def until(end: Offset): IndexRange = indexRange(get, end.get)
  def +(n: Int): Offset              = Offset(get + n)
  def -(n: Int): Offset              = Offset(get - n)
  def unary_- : Offset               = Offset(-get)
  def toIndex: Index                 = Index(get)
  def toNth: Nth                     = toIndex.toNth
  def toOffset: Offset               = this
  private def sign                   = if (get < 0) "-" else if (get > 0) "+" else ""
  def to_s                           = s"$sign$get"
}

object Nth extends (Long => Nth) {
  def undefined: Nth          = new Nth(-1)
  def apply(value: Long): Nth = if (value <= 0) undefined else new Nth(value)
  def unapply(x: Nth): Nth    = x
}
object Offset extends (Int => Offset) {
  def apply(value: Int): Offset  = new Offset(value)
  def unapply(x: Offset): Offset = x
}
