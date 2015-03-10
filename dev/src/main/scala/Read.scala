package psp
package dev

import std._

/** The classic type class for turning string representations into typed values.
 */
trait Read[A] extends Any { def read(x: String): A }

/** For this to have any hope of being smooth, we need the VALUE
 *  (the type class instance) to be inferred, but the TYPE
 *  to be given explicitly. Type inference can't do anything sensible
 *  if the only incoming type is a String.
 *
 *  That's what into[A] is for, to obtain the type up front.
 */
object Read {
  def apply[A](f: String => A): Read[A]                         = new Impl[A](f)
  def unapply[A](s: String)(implicit reads: Read[A]): Option[A] = Try(reads read s).toOption
  def into[A] : ReadInto[A]                                     = new ReadInto[A]

  final class ReadInto[A]() {
    def apply(s: String)(implicit reads: Read[A]): A           = reads read s
    def unapply(s: String)(implicit reads: Read[A]): Option[A] = opt(s)
    def wrap(s: String)(implicit reads: Read[A]): Try[A]       = Try(reads read s)
    def opt(s: String)(implicit reads: Read[A]): Option[A]     = wrap(s).toOption
  }

  final class Impl[A](val f: String => A) extends AnyVal with Read[A] { def read(x: String): A = f(x) }
}
trait ReadInstances {
  implicit def bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit def bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit def doubleRead: Read[Double]     = Read(_.toDouble)
  implicit def floatRead: Read[Float]       = Read(_.toFloat)
  implicit def intRead: Read[Int]           = Read(_.toInt)
  implicit def longRead: Read[Long]         = Read(_.toLong)
  implicit def regexRead: Read[Regex]       = Read(Regex)
  implicit def stringRead: Read[String]     = Read(s => s)
  implicit def uriRead: Read[jUri]          = Read(jUri)
}
