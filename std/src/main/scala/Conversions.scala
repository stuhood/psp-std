package psp
package std

import api._

final class Conversions[A](val xs: View[A]) extends AnyVal with ConversionsImpl[A]

final class Unbuilder[A, Repr](repr: Repr)(implicit z: Unbuilds[A, Repr]) {
  def xs: View[A] = z unbuild repr
  def m: AtomicView[A, Repr] = xs match {
    case xs: Direct[A] => new DirectView(xs)
    case xs            => new LinearView(xs)
  }
}

object Unbuilds {
  def apply[A, Repr](f: Repr => Each[A]): Unbuilds[A, Repr] = new Impl(f)

  final class Impl[A, Repr](val f: Repr => Each[A]) extends AnyVal with Unbuilds[A, Repr] {
    def unbuild(xs: Repr): Each[A] = f(xs)
  }
}

/** Conversions which require the elements to be pairs. Obtaining evidence of that
 *  up front simplifies everything else, because we don't have to mix and match
 *  between arity-1 and arity-2 type constructors.
 */
final class Paired[R, A, B](xs: Each[R])(implicit splitter: Pair.Split[R, A, B]) {
  def toEqualsMap: ExMap[A, B]                                       = toExMap(inheritEq)
  def toExMap(implicit z: Eq[A]): ExMap[A, B]                        = toMap[ExMap]
  def toHashMap(implicit z: Hash[A]): ExMap[A, B]                    = toMap[ExMap]
  def toJavaMap(): jMap[_ >: A, _ <: B]                              = toMap[jMap]
  def toMap[CC[_,_]](implicit z: Builds[A -> B, CC[A, B]]): CC[A, B] = z build (xs map splitter.split)
  def toScalaMap(): sciMap[A, B]                                     = toMap[sciMap]
}

trait ConversionsImpl[A] extends Any {
  def xs: View[A]
  def to[CC[X]](implicit z: Builds[A, CC[A]]): CC[A] = z build xs

  def byEquals: ops.HasEq[A]   = new ops.HasEq[A](xs)(inheritEq)
  def byRef: ops.HasEq[Ref[A]] = new ops.HasEq[Ref[A]](xs.toRefs)(referenceEq)
  def byString: ops.HasEq[A]   = new ops.HasEq[A](xs)(stringEq)

  def toArray(implicit z: CTag[A]): Array[A]   = to[Array]
  def toDirect: Direct[A]                      = to[Direct]
  def toEqualsSet: ExSet[A]                    = toHashSet(inheritEq)
  def toExSet(implicit z: Eq[A]): ExSet[A]     = to[ExSet]
  def toHashSet(implicit z: Hash[A]): ExSet[A] = to[ExSet]
  def toJavaList: jList[A]                     = to[jList]
  def toJavaSet: jSet[A]                       = to[jSet]
  def toLinear: Linear[A]                      = to[Linear]
  def toRefArray(): Array[AnyRef]              = Builds.array[AnyRef] build xs.toRefs
  def toScalaList: sciList[A]                  = to[sciList]
  def toScalaSeq: sciSeq[A]                    = to[sciSeq]
  def toScalaSet: sciSet[A]                    = to[sciSet]
  def toScalaStream: sciStream[A]              = to[sciStream]
  def toScalaVector: sciVector[A]              = to[sciVector]
  def toVec: Vec[A]                            = to[Vec]

  /** Conveniences. */
  def iterator: BiIterator[A] = BiIterator(xs)
  def trav: scTraversable[A]  = to[scTraversable] // flatMap, usually
  def seq: scSeq[A]           = to[scSeq]         // varargs, usually
}
