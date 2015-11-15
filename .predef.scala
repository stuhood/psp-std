import scala.collection.{ mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp._, std._, api._, pio._, jvm._
import StdEq._, StdShow._
import ammonite.repl._
import psprepl._

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
object psprepl extends ReplPackageLow {
  implicit final class ReplForeachOps[A](val xs: Each[A])            extends TargetCommon[A] { def target = xs.toVec }
  implicit final class ReplArrayOps[A](val xs: Array[A])             extends TargetCommon[A] { def target = xs.toVec }
  implicit final class ReplTraversableOps[A](val xs: sCollection[A]) extends TargetCommon[A] { def target = xs.toVec }

  implicit final class ReplMapOps[K, V](val target: ExMap[K, V]) {
    def >(implicit z1: Show[K], z2: Show[V]): ExMap[K, V]  = target doto (m => println(pp"$m"))
    def !>(implicit z1: Show[K], z2: Show[V]): ExMap[K, V] = target doto (_ !>)
  }

  implicit def showToAmmonite[A](implicit z: Show[A]): pprint.PPrinter[A] = pprint.PPrinter[A]((t, c) => scIterator(z show t))
}
