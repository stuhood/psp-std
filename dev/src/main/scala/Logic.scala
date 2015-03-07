package psp
package dev

object Logic {
  object quantifier {
    val universal   = "∀"
    val existential = "∃"
    val uniqueness  = "∃!"
  }
  object connective {
    val tautology     = "⊤"
    val contradiction = "⊥"
    val negation      = "¬"
    val implication   = "→" // "⊃"
    val conjunction   = "∧" //
    val disjunction   = "∨" //
    val biconditional = "↔" // "⇔" "≡"

    val xor  = "⊻"
    val nand = "↑"
    val nor  = "↓"
  }
  val definition = ":="
  val derives    = "⊢" // syntactic consequence
  val entails    = "⊨" // semantic consequence
}
