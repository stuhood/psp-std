package psp
package dmz

import scala._
import scala.{ collection => sc }
import sc.{ mutable => scm, immutable => sci }
import java.lang.{ String, Math }
import sc.convert._

trait PolicyDmz extends AnyRef
      with ScalaLibrary
      with JavaLibrary
      with DecorateAsScala
      with DecorateAsJava

/** Objects mimicking the most commonly used names from scala's
 *  default namespace, since we aren't auto-importing all that
 *  stuff any longer.
 */
object System {
  def out = java.lang.System.out
  def err = java.lang.System.err
  def in  = java.lang.System.in

  def setOut(out: java.io.PrintStream): Unit = java.lang.System.setOut(out)
  def setErr(err: java.io.PrintStream): Unit = java.lang.System.setErr(err)
  def setIn(in: java.io.InputStream): Unit   = java.lang.System.setIn(in)
}
object Console {
  def out = scala.Console.out
  def err = scala.Console.err
  def in  = scala.Console.in

  def putErr(msg: Any): Unit  = try err print msg finally out.flush()
  def putOut(msg: Any): Unit  = try out print msg finally out.flush()
  def echoErr(msg: Any): Unit = err println msg
  def echoOut(msg: Any): Unit = out println msg
}
object sys {
  def error(msg: String): Nothing = scala.sys.error(msg)
  def props                       = scala.sys.props
  def env                         = scala.sys.env
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
object Set {
  def empty[A] : sci.Set[A]        = sci.Set[A]()
  def apply[A](xs: A*): sci.Set[A] = sci.Set[A](xs: _*)
}

object math {
  def max(x: Int, y: Int): Int          = Math.max(x, y)
  def max(x: Long, y: Long): Long       = Math.max(x, y)
  def max(x: Float, y: Float): Float    = Math.max(x, y)
  def max(x: Double, y: Double): Double = Math.max(x, y)

  def min(x: Int, y: Int): Int          = Math.min(x, y)
  def min(x: Long, y: Long): Long       = Math.min(x, y)
  def min(x: Float, y: Float): Float    = Math.min(x, y)
  def min(x: Double, y: Double): Double = Math.min(x, y)

  def abs(x: Int): Int       = Math.abs(x)
  def abs(x: Long): Long     = Math.abs(x)
  def abs(x: Float): Float   = Math.abs(x)
  def abs(x: Double): Double = Math.abs(x)

  def round(x: Float): Int   = Math.round(x)
  def round(x: Double): Long = Math.round(x)
  def round(x: Int): Int     = x
  def round(x: Long): Long   = x

  def signum(x: Float): Float   = Math.signum(x)
  def signum(x: Double): Double = Math.signum(x)
  def signum(x: Int): Int       = if (x < 0) -1 else if (x > 0) 1 else 0
  def signum(x: Long): Long     = if (x < 0L) -1L else if (x > 0L) 1L else 0L

  def IEEEremainder(f1: Double, f2: Double): Double       = Math.IEEEremainder(f1, f2)
  def acos(x: Double): Double                             = Math.acos(x)
  def asin(x: Double): Double                             = Math.asin(x)
  def atan(x: Double): Double                             = Math.atan(x)
  def atan2(y: Double, x: Double): Double                 = Math.atan2(y, x)
  def cbrt(x: Double): Double                             = Math.cbrt(x)
  def ceil(x: Double): Double                             = Math.ceil(x)
  def copySign(magnitude: Double, sign: Double): Double   = Math.copySign(magnitude, sign)
  def cos(x: Double): Double                              = Math.cos(x)
  def cosh(x: Double): Double                             = Math.cosh(x)
  def exp(x: Double): Double                              = Math.exp(x)
  def expm1(x: Double): Double                            = Math.expm1(x)
  def floor(x: Double): Double                            = Math.floor(x)
  def hypot(x: Double, y: Double): Double                 = Math.hypot(x, y)
  def log(x: Double): Double                              = Math.log(x)
  def log10(x: Double): Double                            = Math.log10(x)
  def log1p(x: Double): Double                            = Math.log1p(x)
  def nextAfter(start: Double, direction: Double): Double = Math.nextAfter(start, direction)
  def nextUp(x: Double): Double                           = Math.nextUp(x)
  def pow(x: Double, y: Double): Double                   = Math.pow(x, y)
  def random(): Double                                    = Math.random()
  def rint(x: Double): Double                             = Math.rint(x)
  def sin(x: Double): Double                              = Math.sin(x)
  def sinh(x: Double): Double                             = Math.sinh(x)
  def sqrt(x: Double): Double                             = Math.sqrt(x)
  def tan(x: Double): Double                              = Math.tan(x)
  def tanh(x: Double): Double                             = Math.tanh(x)
  def toDegrees(x: Double): Double                        = Math.toDegrees(x)
  def toRadians(x: Double): Double                        = Math.toRadians(x)
  def ulp(x: Double): Double                              = Math.ulp(x)
}
