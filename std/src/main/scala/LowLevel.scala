package psp
package std
package lowlevel

import api._
import scala.Tuple2
import ArrowAssoc.Types

/** Hand specialized on the left, @specialized on the right, value classes for tuple creation.
 */
final class ArrowAssocInt(val self: Int) extends AnyVal {
  @inline def -> [@spec(Types) B](y: B): Tuple2[Int, B] = Tuple2(self, y)
}
final class ArrowAssocLong(val self: Long) extends AnyVal {
  @inline def -> [@spec(Types) B](y: B): Tuple2[Long, B] = Tuple2(self, y)
}
final class ArrowAssocDouble(val self: Double) extends AnyVal {
  @inline def -> [@spec(Types) B](y: B): Tuple2[Double, B] = Tuple2(self, y)
}
final class ArrowAssocChar(val self: Char) extends AnyVal {
  @inline def -> [@spec(Types) B](y: B): Tuple2[Char, B] = Tuple2(self, y)
}
final class ArrowAssocBoolean(val self: Boolean) extends AnyVal {
  @inline def -> [@spec(Types) B](y: B): Tuple2[Boolean, B] = Tuple2(self, y)
}
final class ArrowAssocRef[A](val self: A) extends AnyVal {
  @inline def -> [B](y: B): Tuple2[A, B] = Tuple2(self, y)
}

final class CircularBuffer[A](capacity: Precise) extends Direct.DirectImpl[A] with AndThis {
  assert(!capacity.isZero, "CircularBuffer capacity cannot be 0")

  private[this] def cap: Int            = capacity.intValue
  private[this] val buffer              = newArray[Any](cap)
  private[this] var seen                = 0L
  private[this] def writePointer: Int   = (seen % cap).safeInt
  private[this] def readPointer         = if (isFull) writePointer else 0
  private[this] def setHead(x: A): Unit = buffer(writePointer) = x sideEffect (seen += 1)

  @inline def foreach(f: A => Unit): Unit = this foreachIndex (i => f(elemAt(i)))

  def head: A                     = elemAt(0.index)
  def isFull                      = seen >= cap
  def elemAt(index: Index): A     = buffer((readPointer + index.getInt) % cap).castTo[A]
  def size: Precise               = capacity min Size(seen)
  def ++=(xs: Each[A]): this.type = andThis(xs foreach setHead)
  def += (x: A): this.type        = andThis(this setHead x)
  def push(x: A): A               = if (isFull) head sideEffect setHead(x) else abort("push on non-full buffer")
}
final class ByteBufferInputStream(b: ByteBuffer) extends InputStream {
  private def empty = !b.hasRemaining

  override def read()                                       = if (empty) -1 else b.get & 0xFF
  override def read(bytes: Array[Byte], off: Int, len: Int) = if (empty) -1 else len min2 b.remaining doto (b.get(bytes, off, _))
}

final class ByteBufferOutputStream(b: ByteBuffer) extends OutputStream {
  override def write(x: Int)                                 = b put x.toByte
  override def write(bytes: Array[Byte], off: Int, len: Int) = b.put(bytes, off, len)
}

object ArrowAssoc {
  val Types = new scala.Specializable.Group((scala.Int, scala.Long, scala.Double, scala.Char, scala.Boolean))
}
object CircularBuffer {
  def builder[A](capacity: Precise): Builds[A, CircularBuffer[A]] = Builds(xs => CircularBuffer[A](capacity) ++= xs)
  def apply[A](capacity: Precise): CircularBuffer[A]              = new CircularBuffer[A](capacity)
}
object Streams {
  def slurp(in: BufferedInputStream): Array[Byte] = {
    val out = new ByteArrayOutputStream
    val buf = new Array[Byte](InputStreamBufferSize)
    def loop(): Array[Byte] = in read buf match {
      case -1 => out.toByteArray
      case n  => out.write(buf, 0, n) ; loop()
    }
    andClose(in)(_ => loop())
  }
  def slurp(in: BufferedInputStream, size: Precise): Array[Byte] = {
    val len = size.getInt
    val buf = newArray[Byte](len)
    def loop(remaining: Int): Array[Byte] = {
      if (remaining == 0) buf
      else in.read(buf, len - remaining, remaining) match {
        case -1 => buf
        case n  => loop(remaining - n)
      }
    }
    andClose(in)(_ => loop(len))
  }
}
