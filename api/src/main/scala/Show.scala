package psp
package api

/** Show.
 */
import Api._

/** The classic type class for turning typed values into string representations.
 */
trait Show[-A] extends Any { def show(x: A): String }
trait Renderer extends Any { def render(x: Doc): String }

/** When a type class is more trouble than it's worth.
 *  Not overriding toString here to leave open the possibility of
 *  using a synthetic toString, e.g. of case classes.
 */
trait ShowDirect extends Any { def to_s: String }
trait ForceShowDirect extends Any with ShowDirect { override def toString = to_s }

/** ADTs composing show logic.
 */
sealed trait TryDoc extends Any
sealed trait Doc extends Any

object TryDoc {
  final case class NoDoc[A](value: A, tag: CTag[A]) extends TryDoc
  final case class HasDoc(doc: Doc) extends TryDoc
}
object Doc {
  final case object NoDoc                             extends Doc
  final case class Group(xs: Direct[Doc])             extends Doc
  final case class Cat(left: Doc, right: Doc)         extends Doc
  final case class Shown[A](value: A, shows: Show[A]) extends Doc
  final case class Literal(value: String)             extends Doc
}
