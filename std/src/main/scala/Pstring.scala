package psp
package std

import api._, StdShow._
import java.{ lang => jl }
import java.util.regex.{ Pattern, Matcher }

final class LinesView(val to_s: String) extends Each[String] with ForceShowDirect {
  private val xs = to_s splitChar '\n'
  private def rebuild(xs: View[String]): LinesView = new LinesView(xs mk_s '\n')

  def collect(pf: String ?=> String): LinesView = rebuild(xs collect pf)
  def filter(p: ToBool[String]): LinesView      = rebuild(xs filter p)
  def foreach(f: String => Unit): Unit          = xs foreach f
  def grep(r: Regex): LinesView                 = rebuild(xs filter r.isMatch)
  def map(f: String => String): LinesView       = rebuild(xs map f)
  def size                                      = xs.size
  def toVec: Vec[String]                        = xs.toVec
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

  def * (n: Int): String     = this * n.size
  def * (n: Precise): String = n.times const self join_s

  def lines: LinesView = new LinesView(self)

  def append(that: String): String                     = self + that   /** Note to self: don't touch this `+`. */
  def bytes: Array[Byte]                               = self.getBytes
  def capitalize: String                               = applyIfNonEmpty[String](self)(s => s.head.toUpper.to_s append s.tail.force)
  def chars: Array[Char]                               = self.toCharArray
  def containsChar(ch: Char): Boolean                  = chars.m contains ch
  def format(args : Any*): String                      = java.lang.String.format(self, args map unwrapArg: _*)
  def length: Int                                      = self.length
  def mapLines(f: ToSelf[String]): String              = lines map f to_s
  def mapChars(pf: Char ?=> Char): String              = self map (c => if (pf isDefinedAt c) pf(c) else c) build
  def mapSplit(ch: Char)(f: ToSelf[String]): String    = splitChar(ch) map f mk_s ch
  def filterSplit(ch: Char)(p: ToBool[String]): String = splitChar(ch) filter p mk_s ch
  def nonEmpty: Boolean                                = onull.length > 0
  def processEscapes: String                           = StringContext processEscapes self
  def remove(regex: Regex): String                     = regex matcher self replaceFirst emptyValue
  def replaceChar(pair: Char -> Char): String          = self.replace(pair._1, pair._2)
  def reverse: String                                  = new String(chars.inPlace.reverse)
  def sanitize: String                                 = mapChars { case x if x.isControl => '?' }
  def splitChar(ch: Char): Vec[String]                 = splitRegex(Regex quote ch.any_s)
  def splitRegex(r: Regex): Vec[String]                = r.pattern split self toVec
  def stripMargin(ch: Char): String                    = lines map (_ remove s"""^${bs}s*[$ch]""".r) to_s
  def stripMargin: String                              = stripMargin('|')
  def stripPrefix(prefix: String): String              = foldPrefix(prefix)(self, s => s)
  def stripSuffix(suffix: String): String              = foldSuffix(suffix)(self, s => s)
  def toBigInt: BigInt                                 = scala.math.BigInt(self)
  def toDecimal: BigDecimal                            = scala.math.BigDecimal(self)
  def toDouble: Double                                 = jl.Double parseDouble dropSuffix(self, "dD")
  def toFloat: Float                                   = jl.Float parseFloat dropSuffix(self, "fF")
  def toInt: Int                                       = foldPrefix("0x")(jl.Integer parseInt self, s => jl.Integer.parseInt(s, 16))
  def toLong: Long                                     = foldPrefix("0x")(jl.Long parseLong dropSuffix(self, "lL"), s => jl.Long.parseLong(dropSuffix(s, "lL"), 16))
  def trimLines: String                                = lines.map(_.trim).to_s.trim

  private def onull = if (self eq null) "" else self
  private def bs    = '\\'

  private def unwrapArg(arg: Any): AnyRef = arg.matchOr(arg.toRef) { case x: ScalaNumber => x.underlying }

  private def foldPrefix[A](prefix: String)(none: => A, some: String => A): A = foldRemove(prefix.r.literal.starts)(none, some)
  private def foldRemove[A](r: Regex)(none: => A, some: String => A): A       = remove(r) match { case `self` => none ; case s => some(s) }
  private def foldSuffix[A](suffix: String)(none: => A, some: String => A): A = foldRemove(suffix.r.literal.ends)(none, some)
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
