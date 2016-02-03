package psp
package tests

import psp.std._, all._, api._, StdShow._, StdEq._
import org.scalacheck._, Prop.forAll, Gen._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }

class CollectionResult(viewOp: ViewClass.Op, testedFn: RecorderCounter => ViewClass) extends ShowSelf {
  val counter = new RecorderCounter
  val xs      = testedFn(counter)
  val name    = xs.name
  val applied = viewOp(xs) take 3
  val result  = applied.to_s
  val calls   = counter.distinctCalls
  val hits    = counter.totalCalls

  def counted = if (isTestDebug) countedDebug else "%3s".format(calls)
  def countedDebug = "%3s/%-4s".format(calls, if (hits == calls) "" else hits)
  def to_s    = fshow"$name%12s  $result%s"
}

final class LabeledFunction[-T, +R](f: T => R, val to_s: String) extends (T ?=> R) with ShowSelf {
  def isDefinedAt(x: T) = f match {
    case f: scala.PartialFunction[_,_] => f isDefinedAt x
    case _                             => true
  }
  def apply(x: T): R = f(x)
}

class OperationCounts extends ScalacheckBundle {
  private[this] var displaysRemaining = maxDisplay

  def bundle                 = "Operation Counts"
  def max                    = 100
  def minSuccessful: Precise = 1000
  def maxDisplay: Precise    = 20
  def numComposite           = 2 upTo 4
  def collections            = vec[RecorderCounter => ViewClass](
    c => PspViewClass("p/direct", Probe.Direct(1 to max, c).view),
    c => ScalaViewClass("s/direct",  Probe.ScalaDirect(1 to max, c).view)
  )
  def chooseMax   = 0 upTo max
  def lowHalf     = 0 upTo max / 2
  def highHalf    = max / 2 upTo max
  def chooseSmall = 1 upTo max / 20
  def chooseRange = gen.indexRangeFrom(max / 2, max)

  private def lop[A](f: A => (String, ViewClass.Op)): A => ViewClass.Op =
    n => f(n) |> { case (label, f) => new LabeledFunction(f, label) }

  private def divides(n: Int)  = (_: Int) % n == 0
  private def less(n: Int)     = (_: Int) < n
  private def multiply(n: Int) = (_: Int) * n

  def viewMethod: Gen[ViewClass.Op] = oneOf(
    lowHalf     ^^ lop(n => s"drop $n"   -> (_ drop n)),
    highHalf    ^^ lop(n => s"take $n"   -> (_ take n)),
    chooseMax   ^^ lop(n => s"dropR $n"  -> (_ dropRight n)),
    chooseMax   ^^ lop(n => s"takeR $n"  -> (_ takeRight n)),
    lowHalf     ^^ lop(n => s"dropW <$n" -> (_ dropWhile less(n))),
    lowHalf     ^^ lop(n => s"takeW <$n" -> (_ takeWhile less(n))),
    chooseSmall ^^ lop(n => s"*$n"       -> (_ map multiply(n))),
    chooseSmall ^^ lop(n => s"/$n"       -> (_ withFilter divides(n))),
    chooseSmall ^^ lop(n => s"!/$n"      -> (_ filterNot divides(n))),
    chooseSmall ^^ lop(n => s"%/$n"      -> (_ collect Partial(divides(n), _ / n))),
    chooseSmall ^^ lop(n => s"x=>(x, x)" -> (_ flatMap (x => vec(x, x)))),
    chooseRange ^^ lop(r => s"slice $r"  -> (_ slice r))
  )
  def composite: Gen[CompositeOp] = viewMethod * numComposite ^^ CompositeOp
  def compositeProp: Prop         = forAll((_: CompositeOp).passed) minSuccessful minSuccessful

  implicit def arbComposite: Arbitrary[CompositeOp] = Arbitrary(composite)

  // final, but this creates an (unsuppressable?) unchecked warning about the outer reference.
  case class CompositeOp(ops: Direct[ViewClass.Op]) {
    lazy val Each(usDirect, themDirect) = outcomes

    lazy val viewOp: ViewClass.Op            = xs => ops.foldl(xs)((res, f) => f(res))
    lazy val outcomes: Vec[CollectionResult] = collections map (f => new CollectionResult(viewOp, f))
    lazy val counts: String                  = compare(usDirect.calls, themDirect.calls)
    lazy val results: Vec[String]            = outcomes mapNow (_.result)

    lazy val failed = (
         results.m.distinct.size =!= 1
      || usDirect.calls > themDirect.calls
    )
    lazy val passed = Try(!failed).fold(
      t => sideEffect(false, t.printStackTrace),
      x => sideEffect(x, maybeShow(x))
    )

    private def maybeShow(passed: Boolean): Unit = {
      if (!passed)
        println(failString)
      else if (isTestDebug || (displaysRemaining > 0 && distinctCounts.size > 1))
        sideEffect(println(passString), displaysRemaining -= 1)
    }

    def compare(lhs: Int, rhs: Int): String = "%3s %-2s %-3s".format(lhs, if (lhs <= rhs) "<=" else ">", rhs)

    def ops_s             = "%-63s" format (ops map ("%-15s" format _.any_s) mk_s ' ')
    def outcomes_s        = outcomes map (_.to_s)
    def description       = if (passed) passString else failString
    def passString        = pp"|$ops_s $counts // ${results.head}"
    def failString        = vec("Inconsistent results for:", ops_s) ++ outcomes_s map (" " + _) joinLines
    def distinctCounts    = outcomes.map(_.calls).byEquals.distinct.toVec
    override def toString = description
  }

  def props() = Vec[NamedProp](
    NamedProp(s"Generating $minSuccessful view combinations, displaying at most $maxDisplay", Prop(true)),
    NamedProp("psp-std never performs more operations than scala", compositeProp)
  )
}
