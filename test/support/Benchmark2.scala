package psp
package tests

import std._, all._, StdShow._

object Benchmark2 {
  var total = 0L

  def toDrop(len: Int) = (len / 9 * 10) + 1
  def b1(xs: Vec[Int]): Long = {
    var i = 0
    var sum = 0L
    while (i < xs.length) {
      sum += ((xs drop i) ++ (xs take i)).last
      i += 1
    }
    sum
  }
  def b2(xs: sciVector[Int]): Long = {
    var i = 0
    var sum = 0L
    while (i < xs.length) {
      sum += ((xs drop i) ++ (xs take i)).last
      i += 1
    }
    sum
  }

  def main(args: Array[String]): Unit = {
    val (max, rounds) = args match {
      case Array(m, r) => m.toInt -> r.toInt
      case Array(m)    => m.toInt -> 1
      case _           => 5000 -> 1
    }
    val r1v = 1 to max toVec
    val r2v = r1v.toScalaVector
    var r1, r2 = 0L

    def do1() = total += timed(r2 += _)(b2(r2v))
    def do2() = total += timed(r1 += _)(b1(r1v))

    1 to rounds foreach { n =>
      if (scala.util.Random.nextBoolean()) { do1 ; do2 } else { do2 ; do1 }
      print(".")
    }
    println(s"..benchmark complete, meaningless sum: $total")
    println("psp-std elapsed: %.3f ms".format(r1 / 1e6))
    println("  scala elapsed: %.3f ms".format(r2 / 1e6))
  }
}
