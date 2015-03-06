package psp
package std

import api._
import StdEq.stringEq
import psp.std.scalac.token.Keyword

final class Label(val label: String) extends AnyVal {
  def matches(r: Regex)   = r isMatch label
  def contains(s: String) = label contains s
  def containsOp          = contains("&&") || contains("||") || (label startsWith "!")
  def isSafe              = matches("""^[(](.*?)[)]$""".r) || !containsOp
  def isBool              = isZero || isOne
  def isZero              = label eq Label.Zero.label
  def isOne               = label eq Label.One.label

  override def toString = label
}
object Label {
  val Zero = new Label(new String(""))
  val One  = new Label(new String(""))
  def apply(s: String) = new Label(s)
}

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any { def to_s: String }
trait ForceShowDirect extends Any with ShowDirect {
  override def toString = to_s
}

class TryShow[-A](shows: Show[A]) {
  def show(x: A): String = if (shows == null) "" + x else shows show x
}
object TryShow {
  implicit def apply[A](implicit z: Show[A] = Show.natural()): TryShow[A] = new TryShow[A](z)
}
final case class TryShown(__shown_rep: String) extends AnyVal {
  override def toString = __shown_rep
}

/** Used to achieve type-safety in the show interpolator.
 *  It's the String resulting from passing a value through its Show instance.
 */
final case class Shown(to_s: String) extends AnyVal with ForceShowDirect {
  def ~ (that: Shown): Shown = new Shown(to_s + that.to_s)
}

object Shown {
  def empty: Shown             = new Shown("")
  def apply(ss: Shown*): Shown = ss.m zreduce (_ ~ _)
}

final class ShowDirectOps(val x: ShowDirect) extends AnyVal {
  /** Java-style String addition without abandoning type safety.
   */
  def + (that: ShowDirect): ShowDirect                = Shown(x.to_s + that.to_s)
  def + [A](that: A)(implicit z: Show[A]): ShowDirect = Shown(x.to_s + (z show that))
}

final class ShowInterpolator(val stringContext: StringContext) extends AnyVal {
  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Shown, which
   *  means they have a Show[A] in scope.
   */
  def show(args: Shown*): String  = StringContext(stringContext.parts: _*).raw(args: _*)
  def pp(args: TryShown*): String = StringContext(stringContext.parts: _*).raw(args: _*)
  def shown(args: Shown*): Shown  = Shown(show(args: _*))
  def doc(args: Doc*): Doc        = stringContext.parts.m.map(_.asis) intersperse args.m joinChars

  /** Can't see any way to reuse the standard (type-safe) f-interpolator, will
   *  apparently have to reimplement it entirely.
   */
  def fshow(args: Shown*): String = (stringContext.parts map (_.processEscapes) mkString "").format(args: _*)

  final def sm(args: Any*): String = {
    def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
    def stripTrailingPart(s: String) = {
      val index: Int   = (s indexWhere isLineBreak).safeInt
      val pre: String  = s take index force
      val post: String = s drop index force;
      pre ~ post.stripMargin
    }
    val stripped: sciList[String] = stringContext.parts.toList match {
      case head :: tail => head.stripMargin :: (tail map stripTrailingPart)
      case Nil          => Nil
    }
    (new StringContext(stripped: _*).raw(args: _*)).trim
  }
}

trait ShowCollections {
  def showEach[A: Show](xs: Each[A]): String
  def showMap[K: Show, V: Show](xs: InMap[K, V]): String
  def showSet[A: Show](xs: InSet[A]): String
  def showJava[A: Show](xs: jIterable[A]): String
  def showScala[A: Show](xs: sCollection[A]): String
}
object ShowCollections {
  implicit object DefaultShowCollections extends DefaultShowCollections

  class DefaultShowCollections extends ShowCollections {
    def maxElements: Precise = 10
    def minElements: Precise = 3

    private def internalEach[A: Show](xs: Each[A]): String = Each.show[A](xs, minElements, maxElements)

    def showEach[A: Show](xs: Each[A]): String = xs match {
      case xs: InSet[A] => showSet(xs)
      case _            => "[ " ~ internalEach[A](xs) ~ " ]"
    }
    def showMap[K: Show, V: Show](xs: InMap[K, V]): String = xs match {
      case xs: ExMap[K, V] => xs.entries.tabular(x => fst(x).to_s, _ => "->", x => snd(x).to_s)
      case xs              => s"$xs"
    }
    def showSet[A: Show](xs: InSet[A]): String = xs match {
      case xs: ExSet[A] => "{ " ~ internalEach[A](xs) ~ " }"
      case _            => InSet show xs
    }
    def showJava[A: Show](xs: jIterable[A]): String    = "j[ " ~ internalEach(Each fromJava xs) ~ " ]"
    def showScala[A: Show](xs: sCollection[A]): String = "s[ " ~ internalEach(Each fromScala xs) ~ " ]"
  }
}

object Show {
  final class Impl[-A](val f: Shower[A]) extends AnyVal with Show[A] { def show(x: A) = f(x) }

  def apply[A](f: Shower[A]): Show[A] = new Impl[A](f)
  def natural[A](): Show[A]           = ToString

  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  private case object ToString extends Show[Any] {
    def show(x: Any): String = x match {
      case null          => ""
      case x: ShowDirect => x.to_s
      case x             => x.toString
    }
  }
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait ShowInstances extends ShowEach {
  implicit def showAttributeName : Show[jAttributeName]       = Show.natural()
  implicit def showBoolean: Show[Boolean]                     = Show.natural()
  implicit def showChar: Show[Char]                           = Show.natural()
  implicit def showClass: Show[jClass]                        = Show(_.shortName)
  implicit def showDirect: Show[ShowDirect]                   = Show(_.to_s)
  implicit def showDouble: Show[Double]                       = Show.natural()
  implicit def showIndex: Show[Index]                         = showBy(_.indexValue)
  implicit def showInt: Show[Int]                             = Show.natural()
  implicit def showLong: Show[Long]                           = Show.natural()
  implicit def showNth: Show[Nth]                             = showBy[Nth](_.nthValue)
  implicit def showOption[A: Show] : Show[Option[A]]          = Show(_.fold("-")(_.to_s))
  implicit def showPath: Show[Path]                           = Show.natural()
  implicit def showScalaNumber: Show[ScalaNumber]             = Show.natural()
  implicit def showStackTraceElement: Show[StackTraceElement] = Show("\tat" + _ + "\n")
  implicit def showString: Show[String]                       = Show.natural()
  implicit def showTuple2[A: Show, B: Show] : Show[(A, B)]    = Show { case (x, y) => show"$x -> $y" }

  implicit def showKeyword: Show[Keyword] = Show[Keyword] {
    case Keyword.Empty            => ""
    case Keyword.ClassConstructor => ""
    case Keyword.ValueParameter   => ""
    case Keyword.TypeParameter    => ""
    case Keyword.Constructor      => "def this"
    case Keyword.PackageObject    => "package object"
    case Keyword.CaseObject       => "case object"
    case Keyword.CaseClass        => "case class"
    case k                        => k.toString.toLowerCase
  }
  implicit def showSize: Show[Size] = Show[Size] {
    case IntSize(size)         => show"$size"
    case LongSize(size)        => show"$size"
    case Bounded(lo, Infinite) => show"$lo+"
    case Bounded(lo, hi)       => show"[$lo,$hi]"
    case Infinite              => "<inf>"
  }
}

trait ShowEach0 {
  implicit def showEach[A: Show, CC[X] <: Each[X]](implicit z: ShowCollections): Show[CC[A]] = Show(z.showEach[A])
}
trait ShowEach1 extends ShowEach0 {
  implicit def showMap[K: Show, V: Show, CC[X, Y] <: InMap[X, Y]](implicit z: ShowCollections): Show[CC[K, V]] = Show(z.showMap[K, V])
  implicit def showSet[A: Show, CC[X] <: InSet[X]](implicit z: ShowCollections): Show[CC[A]]                   = Show(z.showSet[A])
  implicit def showJava [A: Show, CC[X] <: jIterable[X]](implicit z: ShowCollections): Show[CC[A]]             = Show(z.showJava[A])
  implicit def showScala[A: Show, CC[X] <: sCollection[X]](implicit z: ShowCollections): Show[CC[A]]           = Show(z.showScala[A])
}
trait ShowEach extends ShowEach1 {
  implicit def showJavaEnum[A <: jEnum[A]] : Show[jEnum[A]]                    = Show.natural()
  implicit def showArray[A: Show](implicit z: ShowCollections): Show[Array[A]] = showBy[Array[A]](Direct.fromArray)
}


/** For this to have any hope of being smooth, we need the VALUE
 *  (the type class instance) to be inferred, but the TYPE
 *  to be given explicitly. Type inference can't do anything sensible
 *  if the only incoming type is a String.
 *
 *  That's what into[A] is for, to obtain the type up front.
 */
object Read {
  def apply[A](f: String => A): Read[A]                         = new Impl[A](f)
  def unapply[A](s: String)(implicit reads: Read[A]): Option[A] = Try(reads read s).toOption
  def into[A] : ReadInto[A]                                     = new ReadInto[A]

  final class ReadInto[A]() {
    def apply(s: String)(implicit reads: Read[A]): A           = reads read s
    def unapply(s: String)(implicit reads: Read[A]): Option[A] = opt(s)
    def wrap(s: String)(implicit reads: Read[A]): Try[A]       = Try(reads read s)
    def opt(s: String)(implicit reads: Read[A]): Option[A]     = wrap(s).toOption
  }

  final class Impl[A](val f: String => A) extends AnyVal with Read[A] { def read(x: String): A = f(x) }
}
trait ReadInstances {
  implicit def bigDecRead: Read[BigDecimal] = Read(s => BigDecimal(s))
  implicit def bigIntRead: Read[BigInt]     = Read(s => BigInt(s))
  implicit def doubleRead: Read[Double]     = Read(_.toDouble)
  implicit def floatRead: Read[Float]       = Read(_.toFloat)
  implicit def intRead: Read[Int]           = Read(_.toInt)
  implicit def longRead: Read[Long]         = Read(_.toLong)
  implicit def regexRead: Read[Regex]       = Read(Regex)
  implicit def stringRead: Read[String]     = Read(s => s)
  implicit def uriRead: Read[jUri]          = Read(jUri)
}
