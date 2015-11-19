package psp
package std

import api._

/** Yes I know all about implicit classes.
 *  There's no way to write an implicit value class which doesn't hardcode
 *  its location into an object. Separating the implicit conversion from
 *  the class allows clients to build their own package object.
 *
 *  This is all a consequence of scala offering no means for managing namespaces,
 *  so namespace management has become hopelessly entangled with unrelated concerns
 *  like inheritance, specificity, method dispatch, and so forth.
 */
abstract class StdPackage
      extends EmptyInstances
         with PrimitiveInstances
         with AlgebraInstances
         with GlobalShow
         with StdImplicits
         with Aliases
         with psp.dmz.ScalaDmz
         with psp.dmz.JavaDmz {

  // Higher than Direct.
  implicit def arraySpecificOps[A](xs: Array[A]): ops.ArraySpecificOps[A]       = new ops.ArraySpecificOps[A](xs)
  implicit def arrayClassTagOps[A: CTag](xs: Array[A]): ops.ArrayClassTagOps[A] = new ops.ArrayClassTagOps[A](xs)

  implicit def pairedCollectionOps0[R, A, B](xs: View[R])(implicit splitter: Pair.Split[R, A, B]): Paired[R, A, B] = new Paired[R, A, B](xs.toEach)
  implicit def pairedCollectionOps[R, A, B](xs: Each[R])(implicit splitter: Pair.Split[R, A, B]): Paired[R, A, B]  = new Paired[R, A, B](xs)

  // Spire
  type Monoid[A]                  = spire.algebra.Monoid[A]
  type AdditiveMonoid[A]          = spire.algebra.AdditiveMonoid[A]
  type AdditiveSemigroup[A]       = spire.algebra.AdditiveSemigroup[A]
  type MultiplicativeMonoid[A]    = spire.algebra.MultiplicativeMonoid[A]
  type MultiplicativeSemigroup[A] = spire.algebra.MultiplicativeSemigroup[A]
  type BooleanAlgebra[R]          = spire.algebra.Bool[R]
  type UInt                       = spire.math.UInt
  type Natural                    = spire.math.Natural

  implicit class CleaveOps[R, A, B](xs: R)(implicit z: Pair.Cleave[R, A, B]) {
    def mapLeft(f: A => A): R  = z.join(f(z left xs), z right xs)
    def mapRight(f: B => B): R = z.join(z left xs, f(z right xs))
  }
  implicit class PairSplitOps[R, A, B](z: Pair.Split[R, A, B]) {
    def left(x: R): A  = fst(z split x)
    def right(x: R): B = snd(z split x)
  }
  implicit class ApiShowOps[A](val z: Show[A]) {
    def on[B](f: B => A): Show[B] = Show[B](x => z show f(x))
  }
  implicit class ApiEqOps[A](val z: Eq[A]) {
    def on[B](f: B => A): Eq[B] = Eq[B]((x, y) => z.equiv(f(x), f(y)))
  }
  implicit class ApiHashOps[A](val z: Hash[A]) {
    def on[B](f: B => A): Hash[B] = Eq.hash[B]((x, y) => z.equiv(f(x), f(y)))(x => z hash f(x))
  }
  implicit class ApiOrderOps[A](val ord: Order[A]) {
    def |[B: Order](f: A => B): Order[A] = Order((x, y) => ord.compare(x, y) || ?[Order[B]].compare(f(x), f(y)))
    def toEq: Eq[A]                      = Eq[A]((x, y) => ord.compare(x, y) == Cmp.EQ)
    def reverse: Order[A]                = Order[A]((x, y) => ord.compare(x, y).flip)
    def on[B](f: B => A): Order[B]       = Order[B]((x, y) => ord.compare(f(x), f(y)))
  }
  implicit class CmpEnumOps(val cmp: Cmp) {
    def || (that: => Cmp): Cmp = if (cmp == Cmp.EQ) that else cmp
  }
  implicit class BuildsOps[Elem, To](z: Builds[Elem, To]) {
    def comap[Prev](f: Prev => Elem): Builds[Prev, To] = Builds(xs => z build (xs map f))
    def map[Next](f: To => Next): Builds[Elem, Next]   = Builds(xs => f(z build xs))
    def direct: Suspended[Elem] => To                  = mf => z build Each(mf)
    def scalaBuilder: scmBuilder[Elem, To]             = sciVector.newBuilder[Elem] mapResult (z build _.toEach)
  }
  implicit class Tuple2Ops[A, B](val lhs: (A, B)) {
    def fold[C, D](rhs: (A, B))(f: (A, A) => C, g: (B, B) => C)(h: (C, C) => D): D =
      h(f(lhs._1, rhs._1), g(lhs._2, rhs._2))
  }
  implicit class SameTuple2Ops[A](val x: (A, A)) {
    def seq: Vec[A] = vec(x._1, x._2)
  }

  implicit class View2DOps[A](val xss: View2D[A]) {
    def flatten: View[A]              = xss flatMap (_.toEach)
    def mmap[B](f: A => B): View2D[B] = xss map (_ map f)
  }

  def classFilter[A: CTag] : Any ?=> A = ?=>(_.isClass[A], _.castTo[A])

  def transitiveClosure[A: Eq](root: A)(expand: A => Foreach[A]): View[A] = inView { f =>
    def loop(in: View[A], seen: View[A]): Unit = in filterNot seen.contains match {
      case Each() => ()
      case in     => in foreach f ; loop(in flatMap expand, seen ++ in)
    }
    loop(view(root), view())
  }

  implicit def wrapClass(x: jClass): JavaClass                            = new JavaClassImpl(x)
  implicit def wrapClassLoader(x: jClassLoader): JavaClassLoader          = new JavaClassLoaderImpl(x)
  implicit def wrapEnumeration[A](x: jEnumeration[A]): JavaEnumeration[A] = new JavaEnumeration(x)
  implicit def constantPredicate[A](value: Boolean): ToBool[A]            = if (value) ConstantTrue else ConstantFalse
  implicit def conforms[A] : (A <:< A)                                    = new conformance[A]
}
