package psp
package std

import java.util.regex.Pattern
import java.util.regex.Matcher
import api._

final class Regex(val pattern: Pattern) extends AnyVal with ForceShowDirect {
  def matcher(input: jCharSequence): Matcher = pattern matcher input

  def append(e: String)                      = mapRegex(_ + e)
  def capturingGroup                         = surround("(", ")")
  def characterClass                         = surround("[", "]")
  def ends                                   = append("$")
  def flags: Int                             = pattern.flags
  def isMatch(input: jCharSequence): Boolean = matcher(input).matches
  def isMatch[A: Show](x: A): Boolean        = isMatch(x.render)
  def literal                                = surround("\\Q", "\\E") // Not setFlag(LITERAL) lest further regex additions be misinterpreted
  def mapRegex(f: ToSelf[String]): Regex     = Regex(f(to_s), flags)
  def starts                                 = mapRegex("^" + _)
  def surround(s: String, e: String): Regex  = mapRegex(s + _ + e)
  def to_s                                   = pattern.toString

  def |(that: Regex): Regex = mapRegex(_ + "|" + that.pattern)
  def ~(that: Regex): Regex = mapRegex(_ + that.pattern)
}

object Regex extends (String => Regex) {
  def alt(r1: Regex, r2: Regex): Regex = r1 | r2

  def quote(s: String): Regex             = apply(Pattern quote s)
  def apply(s: String): Regex             = new Regex(Pattern compile s)
  def apply(s: String, flags: Int): Regex = new Regex(Pattern.compile(s, flags))
}
