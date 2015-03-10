package psp
package std

import api._

final class Conversions[A](val xs: Each[A]) extends AnyVal {
  /** Generic converters, arrays, functions. */
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A]        = z build xs
  def toScala[CC[X]](implicit z: CanBuild[A, CC[A]]): CC[A] = to[CC](Builds wrap z)
  def toArray(implicit z: CTag[A]): Array[A]                = Array.newBuilder[A] ++= trav result
  def toRefArray(): Array[AnyRef]                           = Array.newBuilder[AnyRef] ++= xs.toRefs.trav result
  def toPartial[K, V](implicit ev: A <:< (K -> V)): K ?=> V = toScalaMap[K, V]

  /** psp collections. */
  def toEach: Each[A]                          = xs
  def toLinear: Linear[A]                      = xs match { case xs: Linear[A] => xs ; case _ => Linear.builder[A] build xs }
  def toDirect: Direct[A]                      = xs match { case xs: Direct[A] => xs ; case _ => Direct.builder[A] build xs }
  def toExSet(implicit z: HashEq[A]): ExSet[A] = xs match { case xs: ExSet[A] => xs  ; case _ => ExSet.builder[A] build xs  }
  def toExMap[K, V](implicit ev: A <:< (K -> V), z: HashEq[K]): ExMap[K, V] = xs match {
    case xs: ExMap[_,_] => xs.castTo[ExMap[K, V]] // suppressing lame patmat warning by not matching on ExMap[K, V]
    case _              => ExMap.builder[K, V] build (xs map ev)
  }

  /** java collections. */
  def toJavaList: jList[A]                                     = jList(seq: _*)
  def toJavaMap[K, V](implicit ev: A <:< (K -> V)): jMap[K, V] = jMap(seq map ev: _*)
  def toJavaSet: jSet[A]                                       = jSet(seq: _*)

  /** scala collections. */
  def toScalaList: sciList[A]                                     = toScala[sciList]
  def toScalaMap[K, V](implicit ev: A <:< (K -> V)): sciMap[K, V] = toScalaVector map ev map (x => x._1 -> x._2) toMap
  def toScalaSeq: sciSeq[A]                                       = toScala[sciSeq]
  def toScalaSet: sciSet[A]                                       = toScala[sciSet]
  def toScalaStream: sciStream[A]                                 = toScala[sciStream]
  def toScalaVector: sciVector[A]                                 = toScala[sciVector]

  /** high-traffic conveniences. */
  def iterator: BiIterator[A] = BiIterator(xs)
  def trav: scTraversable[A]  = toScala[scTraversable] // flatMap, usually
  def seq: scSeq[A]           = toScala[scSeq]         // varargs, usually
}
