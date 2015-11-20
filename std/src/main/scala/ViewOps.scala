package psp
package std
package ops

import api._, StdShow._, StdEq._
import Api.SpecTypes

trait ApiViewOps[+A] extends Any {
  def xs: View[A]

  private def directIsEmpty: Boolean = {
    xs foreach (_ => return false)
    true
  }

  def count(p: ToBool[A]): Int                                              = foldl[Int](0)((res, x) => if (p(x)) res + 1 else res)
  def dropIndex(index: Index): View[A]                                      = xs splitAt index mapRight (_ drop 1) rejoin
  def exists(p: ToBool[A]): Boolean                                         = foldl[Boolean](false)((res, x) => if (p(x)) return true else res)
  def filter(p: ToBool[A]): View[A]                                         = xs withFilter p
  def filterNot(p: ToBool[A]): View[A]                                      = xs withFilter !p
  def find(p: ToBool[A]): Option[A]                                         = foldl(none[A])((res, x) => if (p(x)) return Some(x) else res)
  def first[B](pf: A ?=> B): Option[B]                                      = find(pf.isDefinedAt) map pf
  def foldFrom[@spec(SpecTypes) B](zero: B): HasInitialValue[A, B]          = new HasInitialValue(xs, zero)
  def foldWithIndex[@spec(SpecTypes) B](zero: B)(f: (B, A, Index) => B): B  = foldFrom(zero) indexed f
  def fold[@spec(SpecTypes) B](implicit z: Empty[B]): HasInitialValue[A, B] = foldFrom(z.empty)
  def foldl[@spec(SpecTypes) B](zero: B)(f: (B, A) => B): B                 = foldFrom(zero) left f
  def foldr[@spec(SpecTypes) B](zero: B)(f: (A, B) => B): B                 = foldFrom(zero) right f
  def forall(p: ToBool[A]): Boolean                                         = foldl(true)((res, x) => if (!p(x)) return false else res)
  def forallTrue(implicit ev: A <:< Boolean): Boolean                       = forall(ev)
  def foreachWithIndex(f: (A, Index) => Unit): Unit                         = foldl(0.index)((idx, x) => try idx.next finally f(x, idx))
  def gatherClass[B: CTag] : View[B]                                        = xs collect classFilter[B]
  def grep(regex: Regex)(implicit z: Show[A]): View[A]                      = xs filter (regex isMatch _)
  def head: A                                                               = (xs take 1).toVec.head
  def indexWhere(p: ToBool[A]): Index                                       = zipIndex findLeft p map snd or NoIndex
  def indicesWhere(p: ToBool[A]): View[Index]                               = zipIndex filterLeft p rights
  def init: View[A]                                                         = xs dropRight 1
  def isEmpty: Boolean                                                      = xs.size.isZero || directIsEmpty
  def last: A                                                               = (xs takeRight 1).toVec.head
  def mapApply[B, C](x: B)(implicit ev: A <:< (B => C)): View[C]            = xs map (f => ev(f)(x))
  def mapNow[B](f: A => B): Vec[B]                                          = xs map f toVec
  def mapWithIndex[B](f: (A, Index) => B): View[B]                          = inView[B](mf => foldWithIndex(())((res, x, i) => mf(f(x, i))))
  def mapZip[B](f: A => B): ZipView[A, B]                                   = Zip.zip2(xs, xs map f)
  def join_s(implicit z: Show[A]): String                                   = this mk_s ""
  def mk_s(sep: Char)(implicit z: Show[A]): String                          = this mk_s sep.to_s
  def mk_s(sep: String)(implicit z: Show[A]): String                        = xs map z.show zreducel (_ append sep append _)
  def nonEmpty: Boolean                                                     = xs.size.isNonZero || !directIsEmpty
  def slice(range: IndexRange): View[A]                                     = xs drop range.toDrop take range.toTake
  def sliceWhile(p: ToBool[A], q: ToBool[A]): View[A]                       = xs dropWhile p takeWhile q
  def tail: View[A]                                                         = xs drop 1
  def takeToFirst(p: ToBool[A]): View[A]                                    = xs span !p mapRight (_ take 1) rejoin
  def tee(f: A => String): View[A]                                          = xs map (_ doto (x => println(f(x))))
  def toRefs: View[Ref[A]]                                                  = xs map (_.toRef)
  def withSize(size: Size): View[A]                                         = new Each.Impl[A](size, xs foreach _)
  def zfirst[B](pf: A ?=> B)(implicit z: Empty[B]): B                       = find(pf.isDefinedAt).fold(z.empty)(pf)
  def zfoldl[B](f: (B, A) => B)(implicit z: Empty[B]): B                    = foldl(z.empty)(f)
  def zfoldr[B](f: (A, B) => B)(implicit z: Empty[B]): B                    = foldr(z.empty)(f)
  def zipIndex: ZipView[A, Index]                                           = Zip.zip2[A, Index](xs, Each.indices)
  def zipView[A1, A2](implicit z: Pair.Split[A, A1, A2]): ZipView[A1, A2]   = Zip.zip0[A, A1, A2](xs)(z)
  def zip[B](ys: View[B]): ZipView[A, B]                                    = Zip.zip2[A, B](xs, ys)

  def zipped[L, R](implicit ev: A <:< (L -> R)): ZipView[L, R]     = Zip.zip0(xs)(Pair.Split(ev andThen fst, ev andThen snd))
  def unzip[L, R](implicit ev: A <:< (L -> R)): View[L] -> View[R] = zipped[L, R] |> (x => x.lefts -> x.rights)
}

final class InvariantViewOps[A](val xs: View[A]) extends ApiViewOps[A] {
  def +:(elem: A): View[A] = view(elem) ++ xs
  def :+(elem: A): View[A] = xs ++ view(elem)

  def clusterBy[B: Eq](f: A => B): View[View[A]]      = groupBy[B](f).values
  def gather[B](p: Partial[A, View[B]]): View[B]      = xs flatMap p.zapply
  def sum(implicit z: AdditiveMonoid[A]): A           = z sum xs.trav
  def product(implicit z: MultiplicativeMonoid[A]): A = z prod xs.trav

  /** This is kind of horrific but has the advantage of not being as busted.
   *  We need to not be using scala's map because it will always compare keys based
   *  on the inherited equals.
   */
  def groupBy[B: Eq](f: A => B): ExMap[B, View[A]] = {
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
    val res = buf.toMap map { case (k, v) => seen(k) -> v.m }
    res.m.toMap[ExMap]
  }

  private[this] def orderOps(z: Order[A]): HasOrder[A] = new HasOrder[A](xs)(z)

  def sortBy[B](f: A => B)(implicit z: Order[B]): View[A] = orderOps(orderBy[A](f)).sorted
  def sortWith(cmp: OrderRelation[A]): View[A]            = orderOps(Order(cmp)).sorted
  def findOr(p: ToBool[A], alt: => A): A                  = find(p) | alt

  def reducel(f: BinOp[A]): A = tail.foldl(head)(f)
  def reducer(f: BinOp[A]): A = tail.foldr(head)(f)

  def zinit: View[A]                      = if (isEmpty) emptyValue[View[A]] else init
  def ztail: View[A]                      = if (isEmpty) emptyValue[View[A]] else tail
  def intersperse(ys: View[A]): View[A]   = Split(xs, ys).intersperse
  def cross[B](ys: View[B]): View[A -> B] = for (x <- xs ; y <- ys) yield x -> y

  def transpose[B](implicit ev: A <:< View[B]): View2D[B] = {
    val grid = xs map ev
    def col(n: Index) = grid map (_ drop n.sizeExcluding head)
    Each.indices map col
  }

  def memo: Indexed.Memo[A] = new Indexed.Memo(xs)

  def mpartition(p: View[A] => ToBool[A]): View2D[A] = (
    inView[View[A]](mf => xs.toEach partition p(memo) match {
      case Split(xs, ys) =>
        mf(xs)
        ys mpartition p foreach mf
    })
  )

  def distinctBy[B: Eq](f: A => B): View[A] = inView { mf =>
    zfoldl[ExSet[B]] { (seen, x) =>
      val y = f(x)
      if (seen(y)) seen else {
        mf(x)
        seen + y
      }
    }
  }

  def grouped(n: Precise): View2D[A] = new Grouped[A](n) apply xs

  private def iteratively[B](xs: View[A], head: View[A] => B, tail: ToSelf[View[A]]): View[B] = xs match {
    case _ if xs.isEmpty => emptyValue
    case _               => head(xs) +: iteratively(tail(xs), head, tail)
  }

  def boundedClosure(maxDepth: Precise, f: A => View[A]): View[A] = (
    if (maxDepth.isZero) xs
    else xs ++ (xs flatMap f).boundedClosure(maxDepth - 1, f)
  )
}

private abstract class HeadAndTail[A, B] {
  def isDone(xs: View[A]): Boolean
  def head(xs: View[A]): B
  def tail(xs: View[A]): View[A]
  def apply(xs: View[A]): View[B] = cond(isDone(xs), emptyValue, head(xs) +: apply(tail(xs))) // XXX @tailrec
}

private class Grouped[A](n: Precise) extends HeadAndTail[A, View[A]] {
  def isDone(xs: View[A]): Boolean = xs.isEmpty
  def head(xs: View[A]): View[A]   = xs take n
  def tail(xs: View[A]): View[A]   = xs drop n
}

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
  def distinct: View[A]                           = xs.zfoldl[Vec[A]]((res, x) => if (res.m contains x) res else res :+ x)
  def indexOf(x: A): Index                        = xs indexWhere (_ === x)
  def indicesOf(x: A): View[Index]                = xs indicesWhere (_ === x)
  def mapOnto[B](f: A => B): ExMap[A, B]          = toSet mapOnto f
  def toBag: Bag[A]                               = xs groupBy identity map (_.sizeExact)
  def toSet: ExSet[A]                             = xs.toExSet
  def without(x: A): View[A]                      = xs filterNot (_ === x)
  def withoutEmpty(implicit z: Empty[A]): View[A] = this without z.empty
}
class HasHash[A](xs: View[A])(implicit z: Hash[A]) extends HasEq[A](xs)(z) {
  override def toSet: ExSet[A] = xs.toHashSet
}
final class HasOrder[A](xs: View[A])(implicit z: Order[A]) extends HasEq[A](xs) {
  def max: A                = xs reducel (_ max2 _)
  def min: A                = xs reducel (_ min2 _)
  def sortDistinct: View[A] = sorted.distinct
  def sorted: View[A]       = xs.toRefArray.inPlace.sort
}
final class HasEmpty[A](xs: View[A])(implicit z: Empty[A]) {
  def zapply(i: Index): A      = xs drop i.sizeExcluding zhead
  def zfind(p: ToBool[A]): A   = xs.findOr(p, emptyValue)
  def zhead: A                 = if (xs.isEmpty) emptyValue else xs.head
  def zlast: A                 = if (xs.isEmpty) emptyValue else xs.last
  def zreducel(f: BinOp[A]): A = if (xs.isEmpty) emptyValue else xs reducel f
  def zreducer(f: BinOp[A]): A = if (xs.isEmpty) emptyValue else xs reducer f
}
final class HasInitialValue[+A, B](xs: View[A], initial: B) {
  def indexed(f: (B, A, Index) => B): B = lowlevel.ll.foldLeftIndexed(xs, initial, f)
  def left(f: (B, A) => B): B           = lowlevel.ll.foldLeft(xs, initial, f)
  def right(f: (A, B) => B): B          = lowlevel.ll.foldRight(xs, initial, f)
}
