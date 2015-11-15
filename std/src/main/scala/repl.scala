package psp
package std

import api._, StdShow._

trait ReplPackageLow {
  implicit final class SingleValueReplOps[A](val target: A) {
    def >(implicit z: Show[A]): A = target doto (x => println(x.render))
    def >>(): A                   = target doto (x => println(x.any_s))
  }
}

package object repl extends ReplPackageLow {
  implicit class MultiValueReplOps[A, Repr](repr: Repr)(implicit z: Unbuilds[A, Repr]) {
    private def show(f: A => String): Repr = repr doto (r => z unbuild repr map f foreach println)

    def >(implicit z: Show[A]): Repr = show(_.render)
    def >>(): Repr                   = show(_.any_s)
  }
}
