package psp
package dmz

import scala.Any
import scala.{ collection => sc }
import sc.{ generic => scg, mutable => scm, immutable => sci }

/** Building a default namespace consciously rather than accretively.
 *
 *  The dmz package is working around the flailing which takes place
 *  when you try to alias scala's core types. We need to be completely
 *  clear of the psp.std package object else scala will find a way
 *  to encounter a cycle, which will be handled by issuing no error
 *  and then failing to find identifiers in the near future.
 */
trait ScalaDmz extends Any {
  type BigDecimal  = scala.math.BigDecimal
  type BigInt      = scala.math.BigInt
  type Failure[+A] = scala.util.Failure[A]
  type Success[+A] = scala.util.Success[A]
  type Try[+A]     = scala.util.Try[A]

  // annotations
  type inline    = scala.inline
  type spec      = scala.specialized
  type switch    = scala.annotation.switch
  type tailrec   = scala.annotation.tailrec
  type transient = scala.transient
  type uV        = scala.annotation.unchecked.uncheckedVariance
  type unchecked = scala.unchecked
  type volatile  = scala.volatile

  // scala types which I won't let win.
  type scIterable[+A]         = sc.Iterable[A]
  type scIterator[+A]         = sc.Iterator[A]
  type scMap[K, +V]           = sc.Map[K, V]
  type scSeq[+A]              = sc.Seq[A]
  type scSet[A]               = sc.Set[A]
  type scTraversable[+A]      = sc.Traversable[A]
  type sciIndexedSeq[+A]      = sci.IndexedSeq[A]
  type sciList[+A]            = sci.List[A]
  type sciMap[K, +V]          = sci.Map[K, V]
  type sciSeq[+A]             = sci.Seq[A]
  type sciSet[A]              = sci.Set[A]
  type sciStream[+A]          = sci.Stream[A]
  type sciTraversable[+A]     = sci.Traversable[A]
  type sciVector[+A]          = sci.Vector[A]
  type scmBuilder[-Elem, +To] = scm.Builder[Elem, To]
  type scmMap[K, V]           = scm.Map[K, V]

  // Irregularly named.
  type CanBuild[-Elem, +To] = scg.CanBuildFrom[_, Elem, To]
  type GTOnce[+A]           = sc.GenTraversableOnce[A]
  type sCollection[+A]      = sc.GenTraversable[A]
}
