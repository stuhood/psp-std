package psp
package std

import api._, StdShow._

final case class FunctionGrid[A, B](values: View[A], functions: View[A => B]) {
  def rows: View2D[B]   = values map (v => functions map (f => f(v)))
  def columns:View2D[B] = functions map (f => values map (v => f(v)))

  def renderLines(implicit z: Show[B]): Vec[String]               = {
    val widths    = columns map (_ map z.show map (_.length) max)
    val formatFns = widths map leftFormatString

    rows map (formatFns zip _ map (_ apply _) mk_s ' ')
  }
  def render(implicit z: Show[B]): String = renderLines.joinLines
}

object Grid {
  def apply[A](f: A => (A, A)): A => View[A] = {
    def loop(x: A): View[A] = f(x) |> { case (a, b) => a +: loop(b) }
    loop
  }
}
