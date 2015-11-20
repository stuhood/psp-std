package psp

import org.scalacheck._, Prop._, Gen._
import org.scalacheck.util.Pretty
import psp.std._, api._
import StdShow._
import StdEq._
import scala.Console.{ println => _, _ }

package object tests {
  val PassGreen = GREEN + "\u2713" + RESET // check mark
  val FailRed   = RED + "\u2717" + RESET   // cross mark

  /** Scala, so aggravating.
   *  [error] could not find implicit value for parameter equiv: Eq[A => Bool]
   *  The parameter can be given explicitly, it just won't be found unless the
   *  function type is invariant. The same issue arises with intensional sets.
   */
  type InvariantPredicate[A] = A => Bool

  lazy val isTestDebug = sys.props contains "psp.test.debug"

  implicit def assertions: Assertions = ImmediateTraceAssertions

  type Arb[A]             = Arbitrary[A]
  val Arb                 = Arbitrary
  type Forall1[-A]        = ToBool[A]
  type Forall2[-A]        = Relation[A]
  type Forall3[-A]        = (A, A, A) => Boolean
  type Prop               = org.scalacheck.Prop
  val Prop                = org.scalacheck.Prop
  val Pretty              = org.scalacheck.util.Pretty
  type Result             = org.scalacheck.Test.Result
  type Failed             = org.scalacheck.Test.Failed
  type Buildable[A, C[X]] = org.scalacheck.util.Buildable[A, C[A]]
  type GenParams          = Gen.Parameters
  type TestParams         = Test.Parameters

  def arb[A](implicit z: Arb[A]): Arb[A] = z

  def commutative[T: Arb : Eq](op: BinOp[T]): Prop = forAll((p1: T, p2: T) => printResultIf(false, s"op($p1, $p2)")(sameBehavior(op(p1, p2), op(p2, p1))))
  def associative[T: Arb : Eq](op: BinOp[T]): Prop = forAll((p1: T, p2: T, p3: T) => printResultIf(false, s"op($p1, $p2, $p3)")(sameBehavior(op(op(p1, p2), p3), op(p1, op(p2, p3)))))

  // When testing e.g. associativity and the sum overflows, we
  // need to do more than compare values for equality.
  def sameBehavior[T: Eq](p1: => T, p2: => T): Boolean = {
    import StdEq._
    implicit def t: Eq[Throwable] = eqBy[Throwable](_.getClass)
    Try(p1) === Try(p2)
  }

  implicit class ArbitraryOps[A](x: Arb[A]) {
    def map[B](f: A => B): Arb[B]    = Arb(x.arbitrary map f)
    def filter(p: ToBool[A]): Arb[A] = Arb(x.arbitrary filter p)
  }

  implicit def arbSize: Arb[Size]       = Arb(gen.size)
  implicit def arbWord: Arb[String]     = Arb(gen.text.word)
  implicit def arbitraryPint: Arb[Pint] = Arb(Gen.choose(MinInt, MaxInt) map (x => Pint(x)))
  implicit class LiftConverter[A](gen: Gen[A]) {
    def to[B](implicit f: A => B): Gen[B] = gen map f
  }

  implicit class Gen2Ops[A, B](g: (Gen[A], Gen[B])) {
    def filter(p: (A, B) => Boolean)   = Gen.zip(g._1, g._2) suchThat p.tupled
    def map[C](f: (A, B) => C): Gen[C] = Gen.zip(g._1, g._2) map f.tupled
    def ^^[C](f: (A, B) => C): Gen[C]  = map(f)
  }
  implicit class Gen3Ops[A, B, C](g: (Gen[A], Gen[B], Gen[C])) {
    def filter(f: (A, B, C) => Boolean)   = Gen.zip(g._1, g._2, g._3) suchThat f.tupled
    def map[D](f: (A, B, C) => D): Gen[D] = Gen.zip(g._1, g._2, g._3) map f.tupled
    def ^^[D](f: (A, B, C) => D): Gen[D]  = map(f)
  }
  implicit class TestIntOps(val x: Int) extends AnyVal {
    def upTo(max: Int): Gen[Int] = Gen.choose(x, max)
  }
  implicit class TestLongOps(val x: Long) extends AnyVal {
    def upTo(max: Long): Gen[Long] = Gen.choose(x, max)
  }
  implicit class GenOps[A](g: Gen[A]) {
    def ^^^[B](x: B): Gen[B]                 = Gen const x
    def ^^[B](f: A => B): Gen[B]             = g map f
    def >>[B](f: A => Gen[B]): Gen[B]        = g flatMap f
    def ^?[B](pf: A ?=> B): Gen[B]           = g collect pf
    def *(n: Int): Gen[Direct[A]]            = gen.directOfN(n, g)
    def *(r: Gen[Int]): Gen[Direct[A]]       = r flatMap (g * _)
    def zip[B, C](h: Gen[B])(f: (A, B) => C) = (g, h) map f

    def collect[B](pf: A ?=> B): Gen[B]                                    = g suchThat pf.isDefinedAt map pf.apply
    def collectN[B](n: Int)(pf: Each[A] ?=> B)(implicit z: Arb[A]): Gen[B] = gen.eachOfN(n, g) collect pf
    def stream: Each[A]                                                    = Each continually g.sample flatMap (_.toVec)
    def take(n: Int): Vec[A]                                               = stream take n toVec
  }
  implicit def chooseIndex: Choose[Index]  = Choose.xmap[Long, Index](Index, _.get)
  implicit def chooseSize: Choose[Precise] = Choose.xmap[Long, Precise](Precise, _.get)
  implicit def chooseNth: Choose[Nth]      = Choose.xmap[Long, Nth](Nth, _.get)

  def preNewline(s: String): String                               = if (s containsChar '\n') "\n" + s.mapLines("| " append _) else s
  def showsAs[A: Show](expected: String, x: A): NamedProp         = preNewline(expected) -> (expected =? show"$x")
  def seqShows[A: Show](expected: String, xs: Each[A]): NamedProp = preNewline(expected) -> (expected =? (xs mk_s ", "))

  def expectValue[A](expected: A)(x: A): NamedProp = x.any_s -> (expected =? x)

  def expectType(expected: jClass, found: jClass): NamedProp        = fshow"$expected%15s  >:>  $found%s" -> Prop(expected isAssignableFrom found)
  def expectTypes(expected: jClass, found: Each[jClass]): NamedProp = fshow"$expected%15s  >:>  $found%s" -> found.map(c => Prop(expected isAssignableFrom c))
  def expectType[A: CTag](result: A): NamedProp                     = expectType(classOf[A], result.getClass)
  def expectTypes[A: CTag](results: A*): NamedProp                  = expectTypes(classOf[A], Direct fromScala results mapNow (_.getClass))

  implicit def buildsToBuildable[A, CC[X]](implicit z: Builds[A, CC[A]]): Buildable[A, CC] = new Buildable[A, CC] {
    def builder: scmBuilder[A, CC[A]] = z.scalaBuilder
  }

  implicit class PropOps(p: Prop) {
    def mapParams(f: ToSelf[TestParams]): Prop = new NamedProp.MapParams(p, f)
    def minSuccessful(size: Precise): Prop    = mapParams(_ withMinSuccessfulTests size.toInt)
    def unary_! : Prop                        = p map (r => !r)
  }
  implicit class PropResultOps(r: Prop.Result) {
    def unary_! : Prop.Result = r.copy(status = !r.status)
  }
  implicit class StatusOps(s: Prop.Status) {
    def unary_! : Prop.Status = s match {
      case Proof => False
      case True  => False
      case False => True
      case x     => x
    }
  }

  def printResult[A: Show](msg: String)(result: A): A              = result doto (r => println(show"$msg: $r"))
  def printResultIf[A: Show : Eq](x: A, msg: String)(result: A): A = result doto (r => if (r === x) println(show"$msg: $r"))

  /** How to check for function equivalence? In the absence of mathematical breakthroughs,
   *  recursively throw scalacheck at it again, verifying arbitrary inputs have the same result.
   */
  def observationalEq[M[X], A : Arbitrary, B : Eq](f: (M[A], A) => B): Eq[M[A]] = Eq[M[A]] { (xs, ys) =>
    val prop = forAll((elem: A) => f(xs, elem) === f(ys, elem))
    (Test check prop)(identity).passed
  }
  implicit def pintEq: Hash[Pint]                                     = inheritEq
  implicit def pintShow: Show[Pint]                                   = inheritShow
  implicit def predicateEq[A : Arbitrary] : Eq[InvariantPredicate[A]] = observationalEq[InvariantPredicate, A, Boolean](_ apply _)

  implicit object IdentityAlgebra extends BooleanAlgebra[Boolean] {
    def and(x: Boolean, y: Boolean): Boolean = x && y
    def or(x: Boolean, y: Boolean): Boolean  = x || y
    def complement(x: Boolean): Boolean      = !x
    def zero: Boolean                        = false
    def one: Boolean                         = true
  }
}

package tests {
  final case class Pint(x: Int) { override def toString = s"$x" }
}
