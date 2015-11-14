package psp
package std

import api._

final class Conversions[A](val xs: Each[A]) extends AnyVal with ConversionsImpl[A]

/** Conversions which require the elements to be pairs. Obtaining evidence of that
 *  up front simplifies everything else, because we don't have to mix and match
 *  between arity-1 and arity-2 type constructors.
 */
final class Paired[R, A, B](xs: Each[R])(implicit splitter: Pair.Split[R, A, B]) {
  def toMap[CC[K, V]](implicit z: Builds[A -> B, CC[A, B]]): CC[A, B] = z build (xs map splitter.split)
  def toExMap(implicit z: Eq[A]): ExMap[A, B]                         = toMap[ExMap]
  // def toHashMap(implicit z: HashEq[A]): ExMap[A, B]                   = toMap[ExMap]
  def toJavaMap(): jMap[_ >: A, _ <: B]                               = toMap[jMap]
  def toScalaMap(): sciMap[A, B]                                      = toMap[sciMap]
  def toPartial(): A ?=> B                                            = toScalaMap()
}

trait ConversionsImpl[A] extends Any {
  def xs: Each[A]
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build xs

  def toArray(implicit z: CTag[A]): Array[A] = to[Array]
  def toDirect: Direct[A]                    = to[Direct]
  // def toEach: Each[A]                        = xs
  def toJavaList: jList[A]                   = to[jList]
  def toJavaSet: jSet[A]                     = to[jSet]
  def toLinear: Linear[A]                    = to[Linear]
  def toRefArray(): Array[AnyRef]            = Builds.array[AnyRef] build xs.toRefs
  def toScalaList: sciList[A]                = to[sciList]
  def toScalaSeq: sciSeq[A]                  = to[sciSeq]
  def toScalaSet: sciSet[A]                  = to[sciSet]
  def toScalaStream: sciStream[A]            = to[sciStream]
  def toScalaVector: sciVector[A]            = to[sciVector]

  def toEqualsSet: ExSet[A]                      = toExSet(NaturalEq)
  def toExSet(implicit z: Eq[A]): ExSet[A]       = to[ExSet]
  // def toHashSet(implicit z: HashEq[A]): ExSet[A] = to[ExSet]

  /** Conveniences. */
  def iterator: BiIterator[A]     = BiIterator(xs)
  def trav: scTraversable[A]      = to[scTraversable] // flatMap, usually
  def seq: scSeq[A]               = to[scSeq]         // varargs, usually
}
