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
final class IndexImpl private[std] (val index: Long) extends AnyVal with Index {
  def get: Long                     = index
  def getInt: Int                   = index.safeInt
  def /(size: Int): Index           = this / Size(size)
  def %(size: Int): Index           = this % Size(size)
  def /(size: Precise): Index       = if (isUndefined) this else Index(index / size.value)
  def %(size: Precise): Index       = if (isUndefined) this else Index(index % size.value)
  def +(n: Long): Index             = if (isUndefined) this else Index(index + n)
  def -(n: Long): Index             = if (isUndefined) this else Index(index - n)
  def prev: Index                   = this - 1
  def next: Index                   = this + 1
  def until(end: Index): IndexRange = indexRange(getInt, end.getInt)
  def sizeExcluding: Precise        = index.size
  def sizeIncluding: Precise        = index.size + 1
  def toIndex: Index                = this
  def toNth: Nth                    = Nth(index + 1)
  def toOffset: Offset              = if (isUndefined) abort("undefined") else Offset(getInt)
  def isUndefined                   = index < 0
  def isEmpty                       = isUndefined
  override def toString             = if (isUndefined) "undefined" else s"$index"
}

/** Nth is a 1-based index. The recorded indexValue is 0-based as with Index.
 */
final class Nth private[std] (val nth: Long) extends AnyVal {
  def -(n: Long): Nth   = if (isUndefined) this else Nth(nth - n)
  def +(n: Long): Nth   = if (isUndefined) this else Nth(nth + n)
  def toIndex: Index    = Index(nth - 1)
  def toNth: Nth        = this
  def toOffset: Offset  = toIndex.toOffset
  def getInt: Int       = nth.safeInt
  def isUndefined       = nth <= 0
  override def toString = if (isUndefined) "undefined" else s"#$nth"
}

/** Unlike an Index, an Offset can have any integer value.
 *  A negative offset is a positive offset from the other reference point.
 */
final class Offset private[std] (val offsetValue: Int) extends AnyVal {
  def get                            = offsetValue
  def until(end: Offset): IndexRange = indexRange(offsetValue, end.offsetValue)
  def +(n: Int): Offset              = Offset(offsetValue + n)
  def -(n: Int): Offset              = Offset(offsetValue - n)
  def unary_- : Offset               = Offset(-offsetValue)
  def toSize: Precise                = offsetValue.abs.size
  def toIndex: Index                 = Index(offsetValue)
  def toNth: Nth                     = toIndex.toNth
  def toOffset: Offset               = this
  private def sign                   = if (offsetValue < 0) "-" else if (offsetValue > 0) "+" else ""
  override def toString              = s"$sign$offsetValue"
}

object Index extends (Long => Index) {
  def undefined: Index              = new IndexImpl(-1)
  def zero: Index                   = new IndexImpl(0)
  def apply(value: Long): Index     = if (value < 0) undefined else new IndexImpl(value)
  def unapply(x: api.Index): Index  = x
  def impl(x: api.Index): IndexImpl = new IndexImpl(x.get)
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
