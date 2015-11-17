package psp
package tests

import std._, StdShow._

object Benchmark {
  var total = 0L

  def toDrop(len: Int): Int = (len / 9 * 10) + 1
  def b1(init: Vec[Int]): Long = {
    var xs = init
    var sum = 0L
    while (xs.nonEmpty) {
      sum = sum + xs.foldl(0L)(_ + _)
      xs = xs drop toDrop(xs.length)
    }
    sum
  }
  def b2(init: sciVector[Int]): Long = {
    var xs = init
    var sum = 0L
    while (xs.nonEmpty) {
      sum = sum + xs.foldLeft(0L)(_ + _)
      xs = xs drop toDrop(xs.length)
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    val warmupRounds = 5
    val (max, rounds) = args match {
      case Array(m, r) => m.toInt -> r.toInt
      case Array(m)    => m.toInt -> 1
      case _           => 5000 -> 1
    }
    val r1v = 1 to max toVec
    val r2v = r1v.toScalaVector
    var r1, r2 = 0L

    def do1() = total += timed(r1 += _)(b1(r1v))
    def do2() = total += timed(r2 += _)(b2(r2v))

    -warmupRounds until rounds foreach { n =>
      if (n == 0) {
        print("warmup complete.")
        r1 = 0L
        r2 = 0L
        total = 0L
      }
      if (scala.util.Random.nextBoolean()) { do1 ; do2 } else { do2 ; do1 }
      print(".")
    }
    println(s"..done. $total")
    println("psp-std elapsed: %.3f ms".format(r1 / 1e6))
    println("  scala elapsed: %.3f ms".format(r2 / 1e6))
  }
}
