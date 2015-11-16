package psp

import std._, api._, StdShow._
import ammonite.repl.{ Ref, Repl, Storage }

object ReplMain {
  def main(args: Array[String]): Unit = {
    REPL.start()
  }
}


object REPL extends Repl(System.in, System.out, Ref(Storage(Repl.defaultAmmoniteHome, None)), "", Nil) {
  import interp.replApi._

  private def options     = "-language:_ -Yno-adapted-args -Yno-imports -Yno-predef -encoding UTF-8"
  private def initImports = "import psp._, std._, api._, StdShow._, StdEq._, INREPL._"

  // Working around ammonite bugs.
  // https://github.com/lihaoyi/Ammonite/issues/213
  private def workarounds = "type Order[-A] = psp.api.Order[A] ; val Order = psp.std.Order"

  def start(): Unit = {
    compiler.settings processArgumentString options
    load(initImports)
    load(workarounds)
    run()
  }
  override def action() = {
    val res = super.action()
    printer.println("") // Blank line between results.
    res
  }
}

trait TargetCommon[A] {
  def target: Vec[A]
  def >(implicit z: Show[A]): Vec[A]               = target doto (_ foreach (x => println(x)))
  def !>(implicit z: Show[A], o: Order[A]): Vec[A] = target doto (_.m.sorted foreach (x => println(x)))
}
trait ReplPackageLow {
  implicit final class ReplOps[A](val target: A) {
    def >(implicit z: Show[A]): A = target doto (x => println(z show x))
    def >>(): A                   = target doto (x => anyprintln(x))
  }
  implicit final class ReplJavaOps[A](val xs: jCollection[A]) extends TargetCommon[A] { def target = xs.toVec }
}

object INREPL extends ReplPackageLow {
  implicit final class ReplForeachOps[A](val xs: Each[A])            extends TargetCommon[A] { def target = xs.toVec }
  implicit final class ReplArrayOps[A](val xs: Array[A])             extends TargetCommon[A] { def target = xs.toVec }
  implicit final class ReplTraversableOps[A](val xs: sCollection[A]) extends TargetCommon[A] { def target = xs.toVec }

  implicit final class ReplMapOps[K, V](val target: ExMap[K, V]) {
    def >(implicit z1: Show[K], z2: Show[V]): ExMap[K, V]  = target doto (m => println(pp"$m"))
    def !>(implicit z1: Show[K], z2: Show[V]): ExMap[K, V] = target doto (_ !>)
  }

  implicit def showToAmmonite[A](implicit z: Show[A]): pprint.PPrinter[A] = pprint.PPrinter[A]((t, c) => scIterator(z show t))
}
