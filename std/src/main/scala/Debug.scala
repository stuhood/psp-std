package psp
package std

trait Assertions {
  def failed(msg: => String): Unit
  def assert(assertion: Boolean, msg: => String): Unit = if (!assertion) failed(msg)
}
object Assertions {
  private[this] var instance: Assertions = DefaultAssertions
  def using[A](x: Assertions)(assertion: => Boolean, msg: => String): Unit = {
    val saved = instance
    instance = x
    try instance.assert(assertion, msg) finally instance = saved
  }
  implicit object DefaultAssertions extends Assertions {
    def failed(msg: => String): Unit = assertionError(msg)
  }
}
object ImmediateTraceAssertions extends Assertions {
  def failed(msg: => String): Unit = {
    val t = new AssertionError(msg)
    t.printStackTrace
    throw t
  }
}
