package psp
package std

import api._

/** A valid index is always non-negative. All negative indices are
 *  mapped to NoIndex, which has an underlying value of -1.
 *  Manipulations of invalid values remain invalid, like NaN.
 *  All valid indices give rise to a corresponding Nth which is
 *  one larger, i.e. Index(3) is equivalent to Nth(4).
 */
object Index extends (Long => Index) {
  def invalid: Index                       = new Impl(-1)
  def zero: Index                          = new Impl(0)
  def apply(value: Long): Index            = if (value < 0) invalid else new Impl(value)
  def unapply(x: IndexOrNth): Option[Long] = if (x.isEmpty) none() else some(x.get)
  def impl(x: Index): Impl                 = new Impl(x.get)

  final class Impl private[std] (val indexValue: Long) extends AnyVal with api.Index with ForceShowDirect {
    def %(size: Precise): Index = if (isInvalid) this else Index(get % size.getLong)
    def +(n: Long): Index       = if (isInvalid) this else Index(get + n)
    def -(n: Long): Index       = if (isInvalid) this else Index(get - n)
    def /(size: Precise): Index = if (isInvalid) this else Index(get / size.getLong)

    def get: Long                     = indexValue
    def getInt: Int                   = get.safeInt
    def isEmpty                       = get < 0
    def isInvalid                     = isEmpty
    def next: Index                   = this + 1
    def prev: Index                   = this - 1
    def sizeExcluding: Precise        = Size(get)
    def sizeIncluding: Precise        = Size(get + 1)
    def toIndex: Index                = this
    def toNth: Nth                    = Nth(indexValue + 1)
    def to_s                          = if (isInvalid) "invalid" else s"$get"

    def to(end: Index): IndexRange    = get to end.get map Index
    def until(end: Index): IndexRange = get until end.get map Index
  }
}

/** Nth is a 1-based index.
 */
object Nth extends (Long => Nth) {
  def invalid: Nth                         = new Impl(-1)
  def apply(value: Long): Nth              = if (value <= 0) invalid else new Impl(value)
  def unapply(x: IndexOrNth): Option[Long] = if (x.isEmpty) none() else some(x.get + 1)
  def impl(x: Nth): Impl                   = new Impl(x.get)

  final class Impl private[std] (val nthValue: Long) extends AnyVal with api.Nth with ForceShowDirect {
    def +(n: Long): Nth = if (isInvalid) this else Nth(get + n)
    def -(n: Long): Nth = if (isInvalid) this else Nth(get - n)

    def get: Long      = nthValue
    def getInt: Int    = get.safeInt
    def isEmpty        = get <= 0
    def isInvalid      = isEmpty
    def toIndex: Index = Index(nthValue - 1)
    def toNth: Nth     = this
    def to_s           = if (isInvalid) "undefined" else s"#$get"
  }
}
