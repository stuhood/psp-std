package psp
package std

import api._, StdShow._
import java.{ lang => jl }
import java.util.regex.{ Pattern, Matcher }
import jl.Integer.parseInt, jl.Long.parseLong, jl.Double.parseDouble, jl.Float.parseFloat
import scala.math.ScalaNumber

final class SplitCharView(val xs: Vec[String], sep: Char) extends Direct[String] with ForceShowDirect {
  private def rebuild(xs: Vec[String]) = new SplitCharView(xs, sep)

  def build(): String                               = xs mk_s sep
  def collect(pf: String ?=> String): SplitCharView = rebuild(xs collect pf)
  def elemAt(idx: Index)                            = xs(idx)
  def filter(p: ToBool[String]): SplitCharView      = rebuild(xs filter p)
  def foreach(f: String => Unit): Unit              = xs foreach f
  def grep(r: Regex): SplitCharView                 = rebuild(xs filter r.isMatch)
  def isEmpty: Bool                                 = xs.isEmpty
  def map(f: String => String): SplitCharView       = rebuild(xs map f)
  def size: Precise                                 = xs.size
  def toVec: Vec[String]                            = xs
  def to_s: String                                  = build
}

final class Pstring(val self: String) extends AnyVal with ForceShowDirect {
  // Let this hang out here - uncommenting it reveals which string
  // implicits are being depended upon.
  //
  // private def opsWrapString = null

  def r: Regex     = Regex(self)
  def u: jUrl      = jUrl(self)
  def s: Doc       = Doc.Literal(self)
  def to_s: String = self

  def * (n: Int): String     = this * Size(n)
  def * (n: Precise): String = n.times const self join_s

  def lines: SplitCharView               = splitView('\n')
  def splitView(ch: Char): SplitCharView = new SplitCharView(splitChar(ch), ch)

  def append(that: String): String                  = self + that   /** Note to self: don't touch this `+`. */
  def bytes: Array[Byte]                            = self.getBytes
  def capitalize: String                            = applyIfNonEmpty[String](self)(s => s.head.toUpper.to_s append s.tail.force)
  def chars: Array[Char]                            = self.toCharArray
  def containsChar(ch: Char): Boolean               = chars.m contains ch
  def format(args : Any*): String                   = java.lang.String.format(self, args map unwrapArg: _*)
  def length: Int                                   = self.length
  def mapLines(f: ToSelf[String]): String           = mapSplit('\n')(f)
  def mapChars(pf: Char ?=> Char): String           = chars.m mapPartial pf force
  def mapSplit(ch: Char)(f: ToSelf[String]): String = splitChar(ch) map f mk_s ch
  def processEscapes: String                        = StringContext processEscapes self
  def remove(regex: Regex): String                  = regex matcher self replaceFirst ""
  def replaceChar(pair: Char -> Char): String       = self.replace(pair._1, pair._2)
  def reverse: String                               = new String(chars.inPlace.reverse)
  def sanitize: String                              = mapChars { case x if x.isControl => '?' }
  def splitChar(ch: Char): Vec[String]              = splitRegex(Regex quote ch.any_s)
  def splitRegex(r: Regex): Vec[String]             = r.pattern split self toVec
  def stripMargin(ch: Char): String                 = mapLines(_ remove s"""^${bs}s*[$ch]""".r)
  def stripMargin: String                           = stripMargin('|')
  def stripPrefix(prefix: String): String           = foldPrefix(prefix)(self)(identity)
  def stripSuffix(suffix: String): String           = foldSuffix(suffix)(self)(identity)
  def trimLines: String                             = mapLines(_.trim).trim

  def toBigInt: BigInt      = scala.math.BigInt(self)
  def toDecimal: BigDecimal = scala.math.BigDecimal(self)
  def toDouble: Double      = parseDouble(dropSuffix(self, "dD"))
  def toFloat: Float        = parseFloat(dropSuffix(self, "fF"))
  def toInt: Int            = foldPrefix("0x")(parseInt(self))(parseInt(_, 16))
  def toLong: Long          = foldPrefix("0x")(parseLong(dropSuffix(self, "lL")))(s => parseLong(dropSuffix(s, "lL"), 16))

  private def bs = '\\'
  private def unwrapArg(arg: Any): AnyRef                                     = arg.matchOr(arg.toRef) { case x: ScalaNumber => x.underlying }
  private def foldPrefix[A](prefix: String)(none: => A)(some: String => A): A = foldRemove(prefix.r.literal.starts)(none)(some)
  private def foldRemove[A](r: Regex)(none: => A)(some: String => A): A       = remove(r) match { case `self` => none ; case s => some(s) }
  private def foldSuffix[A](suffix: String)(none: => A)(some: String => A): A = foldRemove(suffix.r.literal.ends)(none)(some)
  private def dropSuffix(s: String, drop: String): String                     = s remove drop.r.characterClass.ends
}

final class Regex(val pattern: Pattern) extends AnyVal with ForceShowDirect {
  def matcher(input: jCharSequence): Matcher = pattern matcher input

  def append(e: String): Regex               = mapRegex(_ + e)
  def capturingGroup: Regex                  = surround("(", ")")
  def characterClass: Regex                  = surround("[", "]")
  def ends: Regex                            = append("$")
  def flags: Int                             = pattern.flags
  def isMatch(input: jCharSequence): Boolean = matcher(input).matches
  def isMatch[A: Show](x: A): Boolean        = isMatch(x.render)
  def literal: Regex                         = surround("\\Q", "\\E") // Not setFlag(LITERAL) lest further regex additions be misinterpreted
  def mapRegex(f: ToSelf[String]): Regex     = Regex(f(to_s), flags)
  def starts: Regex                          = mapRegex("^" + _)
  def surround(s: String, e: String): Regex  = mapRegex(s + _ + e)
  def to_s: String                           = s"$pattern"

  def |(that: Regex): Regex = mapRegex(_ + "|" + that.pattern)
  def ~(that: Regex): Regex = mapRegex(_ + that.pattern)
}

object Regex extends (String => Regex) {
  def alt(r1: Regex, r2: Regex): Regex = r1 | r2

  def quote(s: String): Regex             = apply(Pattern quote s)
  def apply(s: String): Regex             = new Regex(Pattern compile s)
  def apply(s: String, flags: Int): Regex = new Regex(Pattern.compile(s, flags))
}
