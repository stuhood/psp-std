package psp
package std

import api._
import psp.dmz.PolicyDmz

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
      extends impl.EmptyInstances
         with impl.PrimitiveInstances
         with StdProperties
         with impl.AlgebraInstances
         with GlobalShow
         with StdGateways
         with lowlevel.StdArrowAssoc
         with api.Aliases
         with SpireIntegration
         with PolicyDmz {

  // Higher than Direct.
  implicit def arraySpecificOps[A](xs: Array[A]): ops.ArraySpecificOps[A] = new ops.ArraySpecificOps[A](xs)

  implicit def pairedCollectionOps0[R, A, B](xs: View[R])(implicit splitter: Pair.Split[R, A, B]): Paired[R, A, B] = new Paired[R, A, B](xs.toEach)
  implicit def pairedCollectionOps[R, A, B](xs: Each[R])(implicit splitter: Pair.Split[R, A, B]): Paired[R, A, B]  = new Paired[R, A, B](xs)
  // implicit class JavaMap[K, V](val xs: jMap[K, V]) {
  //   def apply(k: K): V = xs get k
  // }
  implicit class CleaveOps[R, A, B](xs: R)(implicit z: Pair.Cleave[R, A, B]) {
    def mapLeft(f: A => A): R  = z.join(f(z left xs), z right xs)
    def mapRight(f: B => B): R = z.join(z left xs, f(z right xs))
  }
  implicit class PairSplitOps[R, A, B](z: Pair.Split[R, A, B]) {
    def left(x: R): A  = fst(z split x)
    def right(x: R): B = snd(z split x)
  }
  implicit class ApiOrderOps[A](val ord: Order[A]) {
    def |[B: Order](f: A => B): Order[A] = Order((x, y) => ord.compare(x, y) || ?[Order[B]].compare(f(x), f(y)))
    def toEq: Eq[A]                      = Eq[A]((x, y) => ord.compare(x, y) == Cmp.EQ)
    // def toHashEq: HashEq[A]              = HashEq natural toEq
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
    def scalaBuilder: scmBuilder[Elem, To]             = sciVector.newBuilder[Elem] mapResult (xs => z build xs)
  }
  implicit class Tuple2Ops[A, B](val lhs: (A, B)) {
    def fold[C, D](rhs: (A, B))(f: (A, A) => C, g: (B, B) => C)(h: (C, C) => D): D =
      h(f(lhs._1, rhs._1), g(lhs._2, rhs._2))
  }
  implicit class SameTuple2Ops[A](val x: (A, A)) {
    def seq: Direct[A] = Direct(x._1, x._2)
  }
  implicit class AnyTargetSeqOps[A: Eq](root: A) {
    def transitiveClosure(expand: A => View[A]): View[A] = inView { f =>
      var seen = exSet[A]()
      def loop(root: A, f: A => Unit): Unit = if (!seen(root)) {
        seen = seen add root
        f(root)
        expand(root) |> (xs => if (xs != null) xs foreach (x => loop(x, f)))
      }
      loop(root, f)
    }
  }

  implicit class View2DOps[A](val xss: View2D[A]) {
    def flatten: View[A]              = xss flatMap (_.toEach)
    def mmap[B](f: A => B): View2D[B] = xss map (_ map f)
  }

  implicit def wrapClass(x: jClass): JavaClass                            = new JavaClassImpl(x)
  implicit def wrapClassLoader(x: jClassLoader): JavaClassLoader          = new JavaClassLoaderImpl(x)
  implicit def wrapEnumeration[A](x: jEnumeration[A]): JavaEnumeration[A] = new JavaEnumeration(x)

  implicit def constantPredicate[A](value: Boolean): ToBool[A] = if (value) ConstantTrue else ConstantFalse
  implicit def conforms[A] : (A <:< A)                         = new conformance[A]
}
