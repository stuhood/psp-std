package psp
package std

import api._, StdShow._

object all extends StdAll {
  final val NoIndex       = Index.invalid
  final val NoFile: jFile = jFile("")
  final val NoPath: jPath = jPath("")
  final val NoUri: jUri   = jUri("")

  implicit final class DocSeqOps(xs: Direct[Doc]) {
    def joinLines: String = xs mapNow (x => render(x)) mk_s EOL
  }

  implicit def foreachDocShows[A: Show](xs: Foreach[A]): DocSeqOps =
    new DocSeqOps(inView[A](xs foreach _) map (x => Doc(x)))
}
