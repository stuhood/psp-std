package psp
package std

import api._
import StdShow._

class FullRenderer extends Renderer {
  def minElements: Precise = 3.size
  def maxElements: Precise = 10.size

  def renderEach(xs: Each[Doc]): String = "[ " + (xs splitAt maxElements.lastIndex match {
    case Split(xs, ys) if ys.isEmpty => xs map (_.render) mk_s ", "
    case Split(xs, _)                => (xs take minElements map (_.render) mk_s ", ") + ", ..."
  }) + " ]"


  def render(x: Doc): String = x match {
    case Doc.NoDoc           => ""
    case Doc.Cat(l, r)       => render(l) + render(r)
    case Doc.Group(xs)       => renderEach(xs)
    case Doc.Shown(value, z) => z show value
    case Doc.Literal(s)      => s
  }
}

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

final class ShowInterpolator(val stringContext: StringContext) extends AnyVal {
  def escapedParts    = stringContext.parts map (_.processEscapes)
  def escaped: String = escapedParts mk_s ""

  /** The type of args forces all the interpolation variables to
   *  be of a type which is implicitly convertible to Doc.
   */

  def show(args: Doc*): String  = StringContext(stringContext.parts: _*).raw(args.map(_.render): _*)
  def pp(args: TryDoc*): String = show(args.map(_.doc): _*)
  def doc(args: TryDoc*): Doc   = Doc.Literal(pp(args: _*))

  /** Can't see any way to reuse the standard (type-safe) f-interpolator, will
   *  apparently have to reimplement it entirely.
   */
  def fshow(args: Doc*): String = escaped.format(args.map(_.render): _*)

  final def sm(args: Any*): String = {
    def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
    def stripTrailingPart(s: String) = {
      val index        = s indexWhere isLineBreak
      val pre: String  = s take index.sizeExcluding force
      val post: String = s drop index.sizeExcluding force;
      pre ~ post.stripMargin
    }
    val stripped: sciList[String] = stringContext.parts.toList match {
      case head :: tail => head.stripMargin :: (tail map stripTrailingPart)
      case Nil          => Nil
    }
    (new StringContext(stripped: _*).raw(args: _*)).trim
  }
}

object Show {
  /** This of course is not implicit as that would defeat the purpose of the endeavor.
   */
  val Inherited: Show[Any] = apply[Any] {
    case null          => ""
    case x: ShowDirect => x.to_s
    case x             => x.toString
  }

  def apply[A](f: ToString[A]): Show[A] = new Impl[A](f)

  final class Impl[-A](val f: ToString[A]) extends AnyVal with Show[A] { def show(x: A) = f(x) }
}

/** An incomplete selection of show compositors.
 *  Not printing the way scala does.
 */
trait ShowInstances extends ShowEach {
  // implicit def showDoc(implicit z: Renderer): Show[Doc] = Show(z render _)
  implicit def showTryDoc : Show[TryDoc] = Show {
    case TryDoc.NoDoc(value, _) => value.any_s
    case TryDoc.HasDoc(x)       => x.render
  }
  implicit def showAttributeName : Show[jAttributeName] = inheritShow
  implicit def showBoolean: Show[Boolean]               = inheritShow
  implicit def showChar: Show[Char]                     = inheritShow
  implicit def showDouble: Show[Double]                 = inheritShow
  implicit def showInt: Show[Int]                       = inheritShow
  implicit def showLong: Show[Long]                     = inheritShow
  implicit def showPath: Show[Path]                     = inheritShow
  implicit def showScalaNumber: Show[ScalaNumber]       = inheritShow
  implicit def showString: Show[String]                 = inheritShow

  implicit def showClass: Show[jClass]                        = Show(_.shortName)
  implicit def showDirect: Show[ShowDirect]                   = Show(_.to_s)
  implicit def showIndex: Show[Index]                         = showBy(_.get)
  implicit def showNth: Show[Nth]                             = showBy[Nth](_.nth)
  implicit def showOption[A: Show] : Show[Option[A]]          = Show(_.fold("-")(_.render))
  implicit def showStackTraceElement: Show[StackTraceElement] = Show("\tat" + _ + "\n")
  implicit def showPair[A: Show, B: Show] : Show[A -> B]      = Show(x => x._1 ~ " -> " ~ x._2 render)
  implicit def showThrowable: Show[Throwable]                 = Show(x => "" + x)

  implicit def showSize: Show[Size] = Show[Size] {
    case IntSize(size)         => show"$size"
    case LongSize(size)        => show"$size"
    case Bounded(lo, Infinite) => show"$lo+"
    case Bounded(lo, hi)       => show"[$lo,$hi]"
    case Infinite              => "<inf>"
  }
}

trait ShowEach0 {
  implicit def showView[A: Show](implicit z: FullRenderer): Show[View[A]] = showBy[View[A]](z renderEach _.toEach.map(_.doc))
}
trait ShowEach1 extends ShowEach0 {
  implicit def showEach[A: Show](implicit z: FullRenderer): Show[Each[A]] = Show(xs => z renderEach (xs map (_.doc)))
}
trait ShowEach extends ShowEach1 {
  implicit def showMap[K: Show, V: Show, CC[X, Y] <: InMap[X, Y]] : Show[CC[K, V]] = Show {
    case xs: ExMap[K, V] => xs.entries.tabular(x => fst(x).render, _ => "->", x => snd(x).render)
    case xs              => s"$xs"
  }
  implicit def showZipped[A1: Show, A2: Show] : Show[ZipView[A1, A2]] = showBy[ZipView[A1, A2]](_.pairs)
  implicit def showArray[A: Show] : Show[Array[A]] = showBy[Array[A]](Direct.fromArray)
  implicit def showJavaEnum[A <: jEnum[A]] : Show[jEnum[A]] = inheritShow
}
