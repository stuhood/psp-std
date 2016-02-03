package psp
package tests

import psp._, std._, all._, api._, StdShow._
import scala.Console.{ println => _, _ }

trait Bundle extends ShowSelf {
  def bundle: String
  def run(): Boolean

  private var count = 0
  private var passed = 0

  def assert(body: => Boolean, msg: => Any): Unit = {
    count += 1
    Try(body).toOption match {
      case Some(true) => passed += 1
      case _          => println(s"Failed: $msg")
    }
  }
  def assert(body: => Boolean): Unit = {
    count += 1
    Try(body).fold(_ => (), res => if (res) passed += 1)
  }

  def finish(msg: String): Boolean = {
    val ok = count == passed
    val color = if (ok) GREEN else RED
    val str = color + "%3s/%-3s".format(passed, count) + RESET
    println(s"$str passed: $msg")
    ok
  }
  def finish(): Boolean = finish(this.shortClass stripSuffix "$")


  def to_s = bundle
}

/** Needed because scalacheck doesn't expose the label if you add
 *  labels with the |: operator.
 */
final class NamedProp(val label: String, p: Prop) {
  def prop = p :| label
  def check: Test.Result = p match {
    case NamedProp.MapParams(prop, f) => Test.check(prop)(f)
    case _                            => Test.check(p)(identity)
  }
}
object NamedProp {
  final case class MapParams(underlying: Prop, f: ToSelf[TestParams]) extends Prop {
    def apply(prms: GenParams)                 = underlying(prms)
    override def check(prms: TestParams): Unit = super.check(f(prms))
  }

  def apply(label: String, p: Prop): NamedProp                 = new NamedProp(label, p)
  implicit def liftSeqPair(x: String -> View[Prop]): NamedProp = NamedProp(x._1, x._2 reducel (_ && _))
  implicit def liftPair(x: String -> Prop): NamedProp          = NamedProp(x._1, x._2)
}

trait ScalacheckBundle extends Bundle {
  def props: Direct[NamedProp]

  def pass = PassGreen
  def fail = FailRed
  def start = "+ " + BOLD + CYAN + bundle + RESET

  def pp(r: Result) = Pretty.pretty(r, Pretty.Params(0))
  def runOne(p: NamedProp): Boolean = p.check match {
    case x if x.passed => sideEffect(true, println("+ %s  %s".format(pass, p.label)))
    case r             => sideEffect(false, println("- %s  %s\nFalsified after %s passed tests\n%s".format(p.label, fail, r.succeeded, pp(r))))
  }

  def run(): Boolean = {
    println("\n" + start)
    props map runOne forall (x => x)
  }

  @Test
  def runBundle(): Unit = junitAssert(run())

  override def toString = bundle
}
