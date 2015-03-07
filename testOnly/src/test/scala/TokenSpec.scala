package psp
package tests

import psp.std._, api._, StdEq._

class TokenSpec extends ScalacheckBundle {
  def bundle = "Scalac Tokens"

  val input            = resourceString("Enumeration.scala")
  val expected: String = resourceString("Enumeration.tokens").trimLines
  val src              = scalac.source(input)
  val toks_s: String   = src.analysis.tokens.tabular("L" + _.line.nth, "" + _.pos.index.index, " " + _.shown).trimLines

  def props: Direct[NamedProp] = Direct(
    s"sample source file tokenizes" -> Prop(expected === toks_s)
  )
}
