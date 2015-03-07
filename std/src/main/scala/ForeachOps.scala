package psp
package std
package ops

import api._

final class ArrayInPlaceOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  private def andThis(op: Unit): Array[A]  = xs
  def size: IntSize                        = Precise(xs.length)
  def map(f: A => A): Array[A]             = andThis(foreachIntIndex(i => xs(i) = f(xs(i))))
  def sort(implicit z: Order[A]): Array[A] = andThis(
    (xs: Array[_]) match {
      case _: Array[AnyRef] => java.util.Arrays.sort[A](xs.castTo[Array[A with AnyRef]], z.toComparator)
      case _                => Array sortInPlace xs
    }
  )
  def reverse(): Array[A] = andThis(
    0 until xs.length / 2 foreach { i =>
      val j = xs.length - 1 - i
      val tmp = xs(j)
      xs(j) = xs(i)
      xs(i) = tmp
    }
  )
}

final class ArraySpecificOps[A](val xs: Array[A]) extends AnyVal with HasPreciseSizeMethods {
  def size: IntSize                          = Precise(xs.length)
  def apply(idx: Index): A                   = xs(idx.safeInt)
  def updated(idx: Index, value: A): xs.type = andThis(xs(idx.safeInt) = value)
  def mapNow[B: CTag](f: A => B): Array[B]   = newArray[B](size) doto (arr => foreachIntIndex(i => arr(i) = f(xs(i))))
  def inPlace: ArrayInPlaceOps[A]            = new ArrayInPlaceOps[A](xs)
  private def andThis(op: Unit): xs.type = xs
}

trait ConversionOps[A] extends Any {
  def xs: Each[A]

  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z build xs
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)

  def toEach: Each[A]                                                     = xs
  def toLinear: Linear[A]                                                 = xs match { case xs: Linear[A] => xs   ; case _ => Linear.builder[A] build xs            }
  def toDirect: Direct[A]                                                 = xs match { case xs: Direct[A] => xs   ; case _ => Direct.builder[A] build xs            }
  def toExSet(implicit z: HashEq[A]): ExSet[A]                            = xs match { case xs: ExSet[A] => xs    ; case _ => ExSet.builder[A] build xs             }
  def toExMap[K, V](implicit ev: A <:< (K, V), z: HashEq[K]): ExMap[K, V] = xs match {
    case xs: ExMap[_,_] => xs.castTo[ExMap[K, V]] // suppressing lame patmat warning by not matching on ExMap[K, V]
    case _              => ExMap.builder[K, V] build (xs map ev)
  }

  def toScalaIterable: scIterable[A]                            = toScala[scIterable]
  def toScalaList: sciList[A]                                   = toScala[sciList]
  def toScalaSet: sciSet[A]                                     = toScala[sciSet]
  def toScalaVector: sciVector[A]                               = toScala[sciVector]
  def toScalaSeq: sciSeq[A]                                     = toScala[sciSeq]
  def toScalaStream: sciStream[A]                               = toScala[sciStream]
  def toScalaTraversable: scTraversable[A]                      = toScala[scTraversable]
  def toScalaMap[K, V](implicit ev: A <:< (K, V)): sciMap[K, V] = toScalaVector map ev toMap
  def toArray(implicit z: CTag[A]): Array[A]                    = Array.newBuilder[A] ++= toScalaTraversable result

  def toPartial[K, V](implicit ev: A <:< (K, V)): K ?=> V = toScalaMap[K, V]

  def toJava: jList[A]                                       = jList(seq: _*)
  def toJavaSet: jSet[A]                                     = jSet(seq: _*)
  def toJavaMap[K, V](implicit ev: A <:< (K, V)): jMap[K, V] = jMap(seq map ev: _*)

  def iterator: scIterator[A] = BiIterator(xs)
  def trav: scTraversable[A]  = toScalaTraversable
  def seq: sciSeq[A]          = toScalaSeq // varargs
}

final class ForeachOps[A](val xs: Each[A]) extends AnyVal with ConversionOps[A] {
  def sized(size: Precise): Each[A] = new Each.Sized(xs, size)
  def memo: Indexed.Memo[A] = xs match {
    case xs: Indexed.Memo[A] => xs
    case _                   => new Indexed.Memo(xs)
  }
}

final class DirectOps[A](val xs: Direct[A]) extends AnyVal with ConversionOps[A] {
  def +:(x: A): Direct[A]          = Direct.prepend(x, xs)
  def :+(x: A): Direct[A]          = Direct.append(xs, x)
  def ++(ys: Direct[A]): Direct[A] = Direct.join(xs, ys)

  def apply(i: Index): A           = xs elemAt i
  def length: Int                  = xs.size.intSize
  def reverse: Direct[A]  = xs match {
    case Direct.Reversed(xs) => xs
    case _                   => new Direct.Reversed(xs)
  }
  def mapNow[B](f: A => B): Direct[B] = {
    val arr = newArray[Any](xs.size)
    xs.indices foreach (i => arr(i.safeInt) = f(xs(i)))
    Direct.wrapArray[B](arr)
  }
}

final class LinearOps[A](val xs: Linear[A]) extends AnyVal with ConversionOps[A] {
  def ::(x: A): Linear[A] = Linear.cons(x, xs)
}
