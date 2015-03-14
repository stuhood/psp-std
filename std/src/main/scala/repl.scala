package psp
package std

import api._
import StdShow._

trait TargetCommon[A] {
  def target: Direct[A]
  def >(implicit z: Show[A]): Direct[A]     = target doto (_ mapNow z.show foreach println)
  def >>(implicit z: TryShow[A]): Direct[A] = target doto (_ mapNow z.show foreach println)
  def !>(implicit z: Show[A]): Direct[A]    = target doto (xs => (xs mapNow z.show).sorted foreach println)
}
trait ReplPackageLow {
  implicit final class ReplOps[A](val target: A) {
    def >(implicit z: Show[A]): A     = target doto (x => println(z show x))
    def >>(implicit z: TryShow[A]): A = target doto (x => println(z show x))
  }
  implicit final class ReplJavaOps[A](val xs: jCollection[A])        extends TargetCommon[A] { def target = xs.toDirect }
}

package object repl extends ReplPackageLow {
  implicit final class ReplForeachOps[A](val xs: Each[A])            extends TargetCommon[A] { def target = xs.toDirect }
  implicit final class ReplArrayOps[A](val xs: Array[A])             extends TargetCommon[A] { def target = xs.toDirect }
  implicit final class ReplTraversableOps[A](val xs: sCollection[A]) extends TargetCommon[A] { def target = xs.toDirect }

  implicit final class ReplMapOps[K, V](val target: ExMap[K, V]) {
    def >(implicit z1: Show[K], z2: Show[V]): ExMap[K, V]        = target doto (m => println(show"$m"))
    def >>(implicit z1: TryShow[K], z2: TryShow[V]): ExMap[K, V] = target doto (m => println(pp"$m"))
    def !>(implicit z1: Show[K], z2: Show[V]): ExMap[K, V]       = target doto (m => m !>)
  }
}
