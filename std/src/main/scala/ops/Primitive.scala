package psp
package std
package ops

import java.{ lang => jl }
import api._

final class AnyOps[A](val x: A) extends AnyVal {
  def any_s: String                         = s"$x"
  def castTo[U] : U                         = x.asInstanceOf[U]
  def collectSelf(pf: A ?=> A): A           = matchOr(x)(pf)
  def id_## : Int                           = java.lang.System.identityHashCode(x)
  def id_==(y: Any): Boolean                = x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]  // Calling eq on Anys.
  def isClass[A: CTag]                      = classOf[A] isAssignableFrom x.getClass
  def matchOpt[B](pf: A ?=> B): Option[B]   = matchOr(none[B])(pf andThen some)
  def matchOr[B](alt: => B)(pf: A ?=> B): B = if (pf isDefinedAt x) pf(x) else alt
  def reflect[B](m: jMethod)(args: Any*): B = m.invoke(x, args.m.toRefs.seq: _*).castTo[B]
  def shortClass: String                    = x.getClass.scalaName.short
  def toRef: Ref[A]                         = castTo[Ref[A]]

  @inline def |>[B](f: A => B): B = f(x)  // The famed forward pipe.
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
  def abs: Int                         = scala.math.abs(self)
  def downTo(end: Int): Direct[Int]    = Consecutive.downTo(self, end)
  def takeNext(len: Precise): IntRange = until(self + len.getInt)
  def to(end: Int): IntRange           = Consecutive.to(self, end)
  def until(end: Int): IntRange        = Consecutive.until(self, end)
}

final class LongOps(val self: Long) extends AnyVal {
  /** Safe in the senses that it won't silently truncate values,
   *  and will translate MaxLong to MaxInt instead of -1.
   *  Note that we depend on this.
   */
  def safeInt: Int = self match {
    case MaxLong => MaxInt
    case MinLong => MinInt
    case _       => assert(self.toInt <= MaxInt, s"$self > $MaxInt") ; self.toInt
  }

  def to(end: Long): LongRange    = self.safeInt to end.safeInt map (_.toLong)
  def until(end: Long): LongRange = self.safeInt until end.safeInt map (_.toLong)
}
