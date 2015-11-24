package psp

import std._, api._, StdShow._
import ammonite.repl.{ Ref, Repl, Storage }
import ammonite.repl.frontend.FrontEnd
import java.lang.System

object ReplMain {
  def main(args: Array[String]): Unit = REPL.start()
}

/** This sets up the ammonite repl with the correct compiler settings
 *  and desired namespace elements and aesthetics.
 */
object REPL extends Repl(System.in, System.out, Ref(Storage(Repl.defaultAmmoniteHome, None)), "", sciSeq()) {
  import interp.replApi._

  private def options     = "-language:_ -Yno-adapted-args -Yno-imports -Yno-predef -encoding UTF-8"
  private def initImports = "import psp._, std._, api._, StdShow._, StdEq._, INREPL._"

  // Working around ammonite bugs.
  // https://github.com/lihaoyi/Ammonite/issues/213
  private def mkNames(name: String, variance: String): String =
    s"""type $name[${variance}A] = psp.api.$name[A] ; val $name = psp.std.$name"""

  private def cov         = vec("Order", "Eq", "Show") map (mkNames(_, "-"))
  private def inv         = vec("Empty") map (mkNames(_, ""))
  private def workarounds = cov ++ inv joinLines

  def start(): Unit = {
    compiler.settings processArgumentString options
    load(initImports)
    load(workarounds)
    frontEnd bind FrontEnd.JLineUnix
    prompt bind "psp> "
    run()
  }
  override def action() = {
    val res = super.action()
    printer.println("") // Blank line between results.
    res
  }
}

/** These classes are imported into the running repl.
 */
object INREPL {
  /** For some type which psp knows how to deconstruct, you can print all its members
   *  to the console by appending > or >> to the creating expression, depending on whether
   *  you want to require a Show[A] instance.
   */
  implicit final class ReplOpsWithShow[A, R](val xs: R)(implicit val z: UnbuildsAs[A, R]) {
    private def run(f: Each[A] => Each[String]): R = sideEffect(xs, f(Each(z unbuild xs foreach _)) foreach (x => println(x)))

    def >                                            = run(_ map (_.any_s))
    def >>(implicit z: Show[A])                      = run(_ map z.show)
    def !>(implicit ord: Order[A], z: Show[A]): Unit = run(_.m.sorted map z.show)
  }

  // implicit def showToAmmonite[A](implicit z: Show[A]): pprint.PPrinter[A] =
  //   pprint.PPrinter[A]((t, c) => BiIterator[String](vec(z show t)))
}
