package psp
package std
package jvm

final class Lazy[+A](body: => A) {
  private[this] var done = false
  lazy val value = try body finally done = true
  def force = value
  def hasRun = done
  def map[B](f: A => B): Lazy[B] = Lazy(f(value))
  override def toString = if (hasRun) s"$value" else "<?>"
}

object Lazy {
  implicit def lowerLazy[A](x: Lazy[A]): A = x.value

  def apply[A](body: => A): Lazy[A]     = new Lazy(body)
  def unapply[A](x: Lazy[A]): Option[A] = if (x.hasRun) Some(x.value) else None
}
