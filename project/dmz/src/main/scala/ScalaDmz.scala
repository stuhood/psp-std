package psp
package dmz

import scala._
import scala.{ collection => sc }
import sc.{ immutable => sci }
import sc.convert._

trait ScalaDmz extends AnyRef
      with ScalaLibrary
      with JavaLibrary
      with DecorateAsScala
      with DecorateAsJava

/** Objects mimicking the most commonly used names from scala's
 *  default namespace, since we aren't auto-importing all that
 *  stuff any longer.
 */
object Console {
  def out = scala.Console.out
  def err = scala.Console.err
  def in  = scala.Console.in

  def putErr(msg: Any): Unit  = try err print msg finally out.flush()
  def putOut(msg: Any): Unit  = try out print msg finally out.flush()
  def echoErr(msg: Any): Unit = err println msg
  def echoOut(msg: Any): Unit = out println msg
}
object :: {
  def apply[A](hd: A, tl: sci.List[A]): sci.::[A] = sci.::(hd, tl)
  def unapply[A](xs: sci.List[A]) = if (xs.isEmpty) None else Some((xs.head, xs.tail))
}
object Try {
  def apply[A](body: => A): scala.util.Try[A] = try Success(body) catch {
    case t: java.lang.ThreadDeath               => throw t
    case t: java.lang.InterruptedException      => throw t
    case t: scala.util.control.ControlThrowable => throw t
    case t: Throwable                           => Failure(t)
  }
}
object Success {
  def apply[A](x: A) = scala.util.Success[A](x)
  def unapply[A](x: scala.util.Try[A]): Option[A] = x match {
    case scala.util.Success(x) => Some(x)
    case _                     => None
  }
}
object Failure {
  def apply(x: Throwable) = scala.util.Failure(x)
  def unapply[A](x: scala.util.Try[A]): Option[Throwable] = x match {
    case scala.util.Failure(x) => Some(x)
    case _                     => None
  }
}
