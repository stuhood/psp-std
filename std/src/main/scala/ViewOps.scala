package psp
package std
package ops

import api._, StdShow._
import Api.SpecTypes

trait ApiViewOps[+A] extends Any {
  def xs: View[A]

  private def stringed(sep: String)(f: ToString[A]): String = xs map f zreduce (_ append sep append _)
  private def directIsEmpty: Boolean = {
    xs foreach (_ => return false)
    true
  }

  def foreachWithIndex(f: (A, Index) => Unit): Unit      = foldl(0.index)((idx, x) => try idx.next finally f(x, idx))
  def count(p: ToBool[A]): Int                           = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)
  def exists(p: ToBool[A]): Boolean                      = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def find(p: ToBool[A]): Option[A]                      = foldl[Option[A]](None)((res, x) => if (p(x)) return Some(x) else res)
  def first[B](pf: A ?=> B): Option[B]                   = find(pf.isDefinedAt) map pf
  def forall(p: ToBool[A]): Boolean                      = foldl[Boolean](true)((res, x) => if (!p(x)) return false else res)
  def forallTrue(implicit ev: A <:< Boolean): Boolean    = forall(x => ev(x))
  def head: A                                            = xs take 1.size optionally { case Each(x) => x } orFail "empty.head"
  def indexWhere(p: ToBool[A]): Index                    = zipIndex findLeft p map snd or NoIndex
  def indicesWhere(p: ToBool[A]): View[Index]            = zipIndex filterLeft p rights
  def isEmpty: Boolean                                   = xs.size.isZero || directIsEmpty
  def last: A                                            = xs takeRight 1.size optionally { case Each(x) => x } orFail "empty.last"
  def mkString(sep: String): String                      = stringed(sep)(_.any_s)
  def mk_s(sep: String)(implicit z: Show[A]): String     = stringed(sep)(_.render)
  def nonEmpty: Boolean                                  = xs.size.isNonZero || !directIsEmpty
  def tabular(columns: ToString[A]*): String             = if (xs.nonEmpty && columns.nonEmpty) FunctionGrid(xs.toVec, columns.m).render else ""
  def toRefs: View[Ref[A]]                               = xs map (_.toRef)
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B    = find(pf.isDefinedAt).fold(z.empty)(pf)
  def zfoldl[B](f: (B, A) => B)(implicit z: Empty[B]): B = foldl(z.empty)(f)
  def zfoldr[B](f: (A, B) => B)(implicit z: Empty[B]): B = foldr(z.empty)(f)

  def tee(f: A => String): View[A] = xs map (_ doto (x => echoOut(f(x))))
  def mapNow[B](f: A => B): Vec[B] = xs map f toVec

  def filter(p: ToBool[A]): View[A]                              = xs withFilter p
  def filterNot(p: ToBool[A]): View[A]                           = xs withFilter !p
  def gather[B](pf: A ?=> View[B]): View[B]                      = xs flatMap pf.zapply
  def gatherClass[B: CTag] : View[B]                             = xs collect classFilter[B]
  def grep(regex: Regex)(implicit z: Show[A]): View[A]           = xs filter (x => regex isMatch x)
  def init: View[A]                                              = xs dropRight 1.size
  def labelOp[B](label: String)(f: View[A] => View[B]): View[B]  = new LabeledView(f(xs), xs.viewOps.castTo[Vec[Doc]] :+ label)
  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): View[C] = xs map (f => ev(f)(x))
  def mapWithIndex[B](f: (A, Index) => B): View[B]               = inView[B](mf => foldWithIndex(())((res, x, i) => mf(f(x, i))))
  def mapZip[B](f: A => B): View[A -> B]                         = xs map (x => x -> f(x))
  def slice(range: IndexRange): View[A]                          = labelOp(pp"slice $range")(_ drop range.toDrop take range.toTake)
  def sliceWhile(p: ToBool[A], q: ToBool[A]): View[A]            = xs dropWhile p takeWhile q
  def tail: View[A]                                              = xs drop 1.size
  def withSize(size: Size): View[A]                              = new Each.Impl[A](size, xs foreach _)

  def zip[B](ys: View[B]): ZipView[A, B]                                  = new Zipped2(xs, ys)
  def zipIndex: ZipView[A, Index]                                         = new Zipped2(xs, Each.indices)
  def zipView[A1, A2](implicit z: Pair.Split[A, A1, A2]): ZipView[A1, A2] = new Zipped0(xs)

  def ofClass[B: CTag] : View[B]         = xs collect classFilter[B]
  def takeToFirst(p: ToBool[A]): View[A] = xs.toEach span !p mapRight (_ take 1.size) rejoin
  def dropIndex(index: Index): View[A]   = xs.toEach splitAt index mapRight (_ drop 1.size) rejoin

  def foldWithIndex[@spec(SpecTypes) B](zero: B)(f: (B, A, Index) => B): B  = foldFrom(zero) indexed f
  def foldl[@spec(SpecTypes) B](zero: B)(f: (B, A) => B): B                 = foldFrom(zero) left f
  def foldr[@spec(SpecTypes) B](zero: B)(f: (A, B) => B): B                 = foldFrom(zero) right f
  def fold[@spec(SpecTypes) B](implicit z: Empty[B]): HasInitialValue[A, B] = foldFrom(z.empty)
  def foldFrom[@spec(SpecTypes) B](zero: B): HasInitialValue[A, B]          = new HasInitialValue(xs, zero)
}

trait InvariantViewOps[A] extends Any with ApiViewOps[A] {
  def +:(elem: A): View[A] = exView(elem) ++ xs
  def :+(elem: A): View[A] = xs ++ exView(elem)

  // We'd love for these not to be here in favor of HasEq and etc, but see the note there.
  def contains(x: A)(implicit z: Eq[A]): Boolean              = exists (_ === x)
  def distinct(implicit z: Eq[A]): View[A]                    = xs.toExSet
  def indexOf(x: A)(implicit z: Eq[A]): Index                 = indexWhere (_ === x)
  def indicesOf(x: A)(implicit z: Eq[A]): View[Index]         = indicesWhere (_ === x)
  def mapOnto[B](f: A => B)(implicit z: Eq[A]): ExMap[A, B]   = xs.toExSet mapOnto f
  def toBag(implicit z: Eq[A]): Bag[A]                        = xs groupBy identity map (_.size)
  def without(x: A)(implicit z: Eq[A]): View[A]               = xs filterNot (_ === x)
  def withoutEmpty(implicit z: Empty[A], eqs: Eq[A]): View[A] = xs without z.empty
  def clusterBy[B: Eq](f: A => B): View[Vec[A]]               = groupBy[B](f).values

  /** This is kind of horrific but has the advantage of not being as busted.
   *  We need to not be using scala's map because it will always compare keys based
   *  on the inherited equals.
   */
  def groupBy[B: Eq](f: A => B): ExMap[B, Vec[A]] = {
    var seen: Vec[B] = vec()
    val buf = bufferMap[Index, Vec[A]]()
    xs foreach { x =>
      val fx: B = f(x)
      val idx: Index = seen indexWhere (fx === _) match {
        case NoIndex => seen = seen :+ fx ; seen.lastIndex
        case idx     => idx
      }
      buf(idx) :+= x
    }
    val res = buf.toMap map { case (k, v) => seen(k) -> v }
    res.m.toMap[ExMap]
  }

  /** Not especially efficient while we're sorting this out.
   *                                       sorting this out.
   *                                       sorting         .
   */
  private[this] def orderOps(implicit z: Order[A]): HasOrder[A] = new HasOrder(xs)

  def max(implicit z: Order[A]): A                        = orderOps.max
  def min(implicit z: Order[A]): A                        = orderOps.min
  def sorted(implicit z: Order[A]): View[A]               = orderOps.sorted
  def sortDistinct(implicit z: Order[A]): View[A]         = orderOps.sortDistinct
  def sortBy[B](f: A => B)(implicit z: Order[B]): View[A] = sorted(z on f)

  def findOr(p: ToBool[A], alt: => A): A            = find(p) | alt
  def reducel(f: BinOp[A]): A                       = tail.foldl(head)(f)
  def zapply(i: Index)(implicit z: Empty[A]): A     = xs drop i.sizeExcluding zhead
  def zfind(p: ToBool[A])(implicit z: Empty[A]): A  = findOr(p, z.empty)
  def zhead(implicit z: Empty[A]): A                = if (isEmpty) z.empty else head
  def zlast(implicit z: Empty[A]): A                = if (isEmpty) z.empty else last
  def zreduce(f: BinOp[A])(implicit z: Empty[A]): A = if (isEmpty) z.empty else reducel(f)

  def zinit: View[A]                    = if (isEmpty) emptyValue[View[A]] else init
  def ztail: View[A]                    = if (isEmpty) emptyValue[View[A]] else tail
  def prepend(x: A): View[A]            = exView(x) ++ xs
  def append(x: A): View[A]             = xs ++ exView(x)
  def intersperse(ys: View[A]): View[A] = xs zip ys flatMap ((x, y) => vec(x, y))

  def transpose[B](implicit ev: A <:< View[B]): View2D[B] = {
    val grid = xs map ev
    def col(n: Index) = grid map (_ drop n.sizeExcluding head)
    Each.indices map col
  }

  def mpartition(p: View[A] => ToBool[A]): View2D[A] = (
    inView[View[A]](mf => xs.toEach partition p(xs.toEach.memo) match {
      case Split(xs, ys) =>
        mf(xs)
        ys mpartition p foreach mf
    })
  )

  def distinctBy[B: Eq](f: A => B): View[A] = inView { mf =>
    zfoldl[ExSet[B]]((seen, x) => f(x) |> (y => try seen add y finally seen(y) || mf(x)))
  }

  def grouped(n: Precise): View2D[A] = new Grouped[A](n) apply xs

  private def iteratively[B](xs: View[A], head: View[A] => B, tail: ToSelf[View[A]]): View[B] = xs match {
    case _ if xs.isEmpty => emptyValue
    case _               => head(xs) +: iteratively(tail(xs), head, tail)
  }

  def boundedClosure(maxDepth: Precise, f: A => View[A]): View[A] = (
    if (maxDepth.isZero) xs
    else xs ++ (xs flatMap (x => f(x).toEach)).boundedClosure(maxDepth - 1, f)
  )
}

private abstract class HeadAndTail[A, B] {
  def isDone(xs: View[A]): Boolean
  def head(xs: View[A]): B
  def tail(xs: View[A]): View[A]
  def apply(xs: View[A]): View[B] = if (isDone(xs)) emptyValue else head(xs) +: apply(tail(xs)) // XXX @tailrec
}
private class Grouped[A](n: Precise) extends HeadAndTail[A, View[A]] {
  def isDone(xs: View[A]): Boolean = xs.isEmpty
  def head(xs: View[A]): View[A]   = xs take n
  def tail(xs: View[A]): View[A]   = xs drop n
}

final class ViewOpsImpl[A](val xs: View[A]) extends InvariantViewOps[A]

final class InvariantApiViewOps[A](val xs: InvariantView[A]) extends AnyVal with InvariantViewOps[A] { }

/** Methods requiring us to have additional knowledge, by parameter or type class.
 *  We keep the interface simple and foolproof by establishing thet instance
 *  first and only offering the methods after that.
 *
 *  But.
 *
 *  This approach, so nice in principle, stretches scala's willingness to connect
 *  implicits past its abilities. It works on psp collections, but when we depend on
 *  an implicit to entire Viewville in the first place, then these methods become
 *  out of reach without an implicit call to .m to become a view.
 *
 *  The search continues.
 */
class HasEq[A](xs: View[A])(implicit z: Eq[A]) {
  def contains(x: A): Boolean                     = xs exists (_ === x)
  def distinct: View[A]                           = xs.foldl(vec[A]())((res, x) => if (res exists (_ === x)) res else res :+ x).m
  def indexOf(x: A): Index                        = xs indexWhere (_ === x)
  def indicesOf(x: A): View[Index]                = xs indicesWhere (_ === x)
  def mapOnto[B](f: A => B): ExMap[A, B]          = toSet mapOnto f
  def toBag: Bag[A]                               = xs groupBy identity map (_.size)
  def toSet: ExSet[A]                             = xs.toExSet
  def without(x: A): View[A]                      = xs filterNot (_ === x)
  def withoutEmpty(implicit z: Empty[A]): View[A] = this without z.empty
}
final class HasOrder[A](xs: View[A])(implicit z: Order[A]) extends HasEq[A](xs) {
  def max: A                = xs reducel (_ max2 _)
  def min: A                = xs reducel (_ min2 _)
  def sortDistinct: View[A] = sorted.distinct
  def sorted: View[A]       = xs.toRefArray.inPlace.sort
}

final class HasInitialValue[+A, B](xs: View[A], initial: B) {
  def indexed(f: (B, A, Index) => B): B = lowlevel.ll.foldLeftIndexed(xs, initial, f)
  def left(f: (B, A) => B): B           = lowlevel.ll.foldLeft(xs, initial, f)
  def right(f: (A, B) => B): B          = lowlevel.ll.foldRight(xs, initial, f)
}
