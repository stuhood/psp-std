package psp
package dev

import std._

sealed trait Ev[+A] extends Any {
  def get: A
  def isEmpty = false
}
final class ByValue[A](val get: A) extends Ev[A] { override def toString = s"$get"                        }
final class ByName[A](f: => A)     extends Ev[A] { def get: A = f ; override def toString = s"$get"       }
final class ByNeed[A](f: => A)     extends Ev[A] { lazy val get: A = f ; override def toString = "<lazy>" }

object Ev {
  def apply[A](body: => A): Ev[A] = scala.util.Try(byValue(body)) match {
    case scala.util.Success(x) => x
    case _                     => byNeed(body)
  }
  def byName[A](body: => A): Ev[A]  = new ByName(body)
  def byNeed[A](body: => A): Ev[A]  = new ByNeed(body)
  def byValue[A](body: => A): Ev[A] = new ByValue(body)

  def unappy[A](x: Ev[A]): Ev[A] = x
}

trait Represent[EC[X], LC[X]] {
  def eagerly[A](x: A): EC[A]
  def lazily[A](x: => A): LC[A]
  def forgetfully[A](x: => A): LC[A]
}

object Represent {
  implicit object RepresentEv extends Represent[Id, Ev] {
    def eagerly[A](x: A): A            = x
    def lazily[A](x: => A): Ev[A]      = Ev byNeed x
    def forgetfully[A](x: => A): Ev[A] = Ev byName x
  }
}
