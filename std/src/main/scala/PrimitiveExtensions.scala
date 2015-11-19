package psp
package std
package ops

import java.{ lang => jl }
import api._

final class AnyOps[A](val x: A) extends AnyVal {
  def any_s: String                         = s"$x"
  def castTo[U] : U                         = x.asInstanceOf[U]
  def id_## : Int                           = java.lang.System.identityHashCode(x)
  def id_==(y: Any): Boolean                = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]  // Calling eq on Anys.
  def isClass[A: CTag]                      = classOf[A] isAssignableFrom x.getClass
  def matchOr[B](alt: => B)(pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else alt
  def matchOpt[B](pf: A ?=> B): Option[B]   = matchOr(none[B])(pf andThen some)
  def reflect[B](m: jMethod)(args: Any*): B = m.invoke(x, args.m.toRefs.seq: _*).castTo[B]
  def shortClass: String                    = NameTransformer decode (x.getClass.getName splitChar '.').last
  def toRef: Ref[A]                         = castTo[Ref[A]]

  @inline def doto(f: A => Unit): A  = sideEffect[A](x, f(x))
  @inline def |>[B](f: A => B): B    = f(x)  // The famed forward pipe.
}

final class AnyRefOps[A <: AnyRef](val x: A) extends AnyVal {
  @inline def foldNull[B](zero: => B)(f: A => B): B = if (x eq null) zero else f(x)
  @inline def doto(f: A => Unit): x.type            = sideEffect[x.type](x, f(x))
}

final class CharOps(val ch: Char) extends AnyVal {
  def isAlphabetic = jl.Character isAlphabetic ch
  def isControl    = jl.Character isISOControl ch
  def isDigit      = jl.Character isDigit ch
  def isLetter     = jl.Character isLetter ch
  def isLower      = jl.Character isLowerCase ch
  def isUpper      = jl.Character isUpperCase ch
  def toLower      = jl.Character toLowerCase ch
  def toUpper      = jl.Character toUpperCase ch
  def to_s         = ch.toString
}

final class IntOps(val self: Int) extends AnyVal {
  private type This = Int

  def offset: Offset = Offset(self)
  def size: Precise  = Size(self)

  /** Make a 64-bit long by concatenating two 32-bit Ints.
   *  Retrieve the original Ints with left32 and right32.
   */
  @inline def join64(that: Int): Long = (self.toLong << 32) | that.toLong

  def abs: This               = scala.math.abs(self)
  def max(that: This): This   = scala.math.max(self, that)
  def min(that: This): This   = scala.math.min(self, that)
  def signum: This            = scala.math.signum(self)
  def lowerBound(n: Int): Int = max(n)
  def zeroPlus: Int           = lowerBound(0)

  def isOr(p: Int => Boolean)(alt: => Int): Int = if (p(self)) self else alt

  def until(end: Int): IntRange        = Consecutive.until(self, end)
  def to(end: Int): IntRange           = Consecutive.to(self, end)
  def downTo(end: Int): Direct[Int]    = Consecutive.downTo(self, end)
  def takeNext(len: Precise): IntRange = Consecutive.until(self, self + len.toInt)

  def binary: String = jl.Integer.toBinaryString(self)
  def hex: String    = jl.Integer.toHexString(self)
  def octal: String  = jl.Integer.toOctalString(self)
}

final class LongOps(val self: Long) extends AnyVal {
  private type This = Long

  def nth: Nth      = Nth(self)
  def index: Index  = Index(self)
  def size: Precise = api.Size(self)

  def abs: This                 = scala.math.abs(self)
  def max(that: This): This     = scala.math.max(self, that)
  def min(that: This): This     = scala.math.min(self, that)
  def signum: This              = scala.math.signum(self)
  def lowerBound(n: Long): Long = max(n)
  def zeroPlus: Long            = lowerBound(0)
  def isNonZero: Boolean        = self != 0L

  def left32: Int  = (self >>> 32).toInt
  def right32: Int = self.toInt

  /** Safe in the senses that it won't silently truncate values, and
   *  will translate MaxLong to MaxInt instead of -1.
   */
  def safeInt: Int = self match {
    case MaxLong => MaxInt
    case MinLong => MinInt
    case _       =>
      assert(self <= (MaxInt: Long), s"$self < $MaxInt")
      self.toInt
  }

  def binary: String = jl.Long.toBinaryString(self)
  def hex: String    = jl.Long.toHexString(self)
  def octal: String  = jl.Long.toOctalString(self)
}
