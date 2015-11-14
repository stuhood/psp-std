package psp
package dev

import std._
// import java.lang.Math

object TransducerUniversal {
  type Reduct[-A, R] = (R, A) => R
  trait Trans[+A, -B] { def apply[R](f: Reduct[A, R]): Reduct[B, R] }

  def map[A, B](f: A => B): Trans[B, A]                            = new Trans[B, A] { def apply[R](rf: Reduct[B, R]) = (r, a) => rf(r, f(a)) }
  def filter[A](p: A => Boolean): Trans[A, A]                      = new Trans[A, A] { def apply[R](rf: Reduct[A, R]) = (r, a) => if (p(a)) rf(r, a) else r }
  def comp[A,B,C](t1 : Trans[A, B], t2 : Trans[C, A]): Trans[C, B] = new Trans[C, B] { def apply[R](rf: Reduct[C, R]) = t1(t2(rf)) }
  def sequence[A, B](t: Trans[B, A], data: scSeq[A])               = data.foldLeft(scSeq[B]())(t(_ :+ _))

  implicit class Compable[A,B](t1: Trans[A, B]) {
    def compose[C](t2: Trans[C, A]): Trans[C, B] = comp(t1, t2)
    def ∘[C](t2: Trans[C, A]): Trans[C, B]       = compose(t2)
    def transform[R](rf: Reduct[A, R])           = t1(rf)
    def ⟐[R]                                     = transform[R] _
  }

  def ⋅[A,B,C](t1: Trans[A, B], t2: Trans[C, A]): Trans[C, B] = comp(t1,t2)
  def o[A,B,C](t1: Trans[A, B], t2: Trans[C, A]): Trans[C, B] = comp(t1,t2)
}

object Test {
  import TransducerUniversal._

  def main(args: Array[String]): Unit = {
    val t_parsei: Trans[Int, String] = map(_.toInt)
    val t_root2 : Trans[Double, Int] = map(i => math.pow(2.0, 1.0 / i))
    def t_repeat[A, R]               = new Trans[A, A] { def apply[R](rf: Reduct[A, R]) = (r, a) => rf(rf(r, a), a) }

    anyprintln(sequence(t_parsei ∘ t_repeat ∘ t_root2, sciList("1", "2", "3")))
    anyprintln(sciList("1", "2", "3").foldLeft(0.0d)(t_parsei ∘ t_repeat ∘ t_root2 ⟐ (_ + _)))
  }
}
