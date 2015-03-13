package psp
package std

import api._
import StdShow._

trait ReplPackageLow {
  implicit final class SingleValueReplOps[A](val target: A) {
    def >(implicit z: Show[A]): A = target doto (x => println(x.doc.render))
    def >>(): A                   = target doto (x => println(x.anydoc.render))
  }
}

package object repl extends ReplPackageLow {
  implicit class MultiValueReplOps[A, Repr](repr: Repr)(implicit z: Unbuilds[A, Repr]) {
    private def xs: View[A] = z unbuild repr

    def >(implicit z: Show[A]):  View[A] = xs tee (_.doc)
    def >>(): View[A]                    = xs tee (_.anydoc)
  }
}
