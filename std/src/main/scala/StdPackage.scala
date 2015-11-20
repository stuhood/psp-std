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

  implicit val defaultRenderer: FullRenderer              = new FullRenderer
  implicit def docOrder(implicit z: Renderer): Order[Doc] = {
    implicit def lexical = lexicalOrder
    orderBy[Doc](x => render(x))
  }

  implicit class DocOps(val lhs: Doc) {
    def doc: Doc                             = lhs
    def render(implicit z: Renderer): String = z show lhs
    def isEmpty: Boolean                     = lhs eq emptyValue[Doc]

    def ~(rhs: Doc): Doc   = Doc.Cat(lhs, rhs)
    def <>(rhs: Doc): Doc  = if (lhs.isEmpty) rhs else if (rhs.isEmpty) lhs else lhs ~ rhs
    def <+>(rhs: Doc): Doc = if (lhs.isEmpty) rhs else if (rhs.isEmpty) lhs else lhs ~ " ".s ~ rhs
  }

  implicit class DirectOps[A](val xs: Direct[A]) {
    def apply(i: Index): A = xs elemAt i
  }
  implicit class Tuple2Ops[A, B](val lhs: (A, B)) {
    def fold[C, D](rhs: (A, B))(f: (A, A) => C, g: (B, B) => C)(h: (C, C) => D): D =
      h(f(lhs._1, rhs._1), g(lhs._2, rhs._2))
  }
  implicit class SameTuple2Ops[A](val x: (A, A)) {
    def seq: Vec[A] = vec(x._1, x._2)
  }

  implicit class HasSizeOps(val xs: HasSize) { //extends AnyVal {
    def sizeLong: Long = sizeExact.longValue
    def sizeInt: Int   = sizeExact.intValue
    def sizeExact: Precise = xs.size match {
      case x: Precise => x
      case n          => abort(s"$n")
    }
  }

  implicit class View2DOps[A](val xss: View2D[A]) {
    def flatten: View[A]              = xss flatMap (_.toEach)
    def mmap[B](f: A => B): View2D[B] = xss map (_ map f)
  }
  implicit def wrapClass(x: jClass): JavaClass                            = new JavaClassImpl(x)
  implicit def wrapClassLoader(x: jClassLoader): JavaClassLoader          = new JavaClassLoaderImpl(x)
  implicit def wrapEnumeration[A](x: jEnumeration[A]): JavaEnumeration[A] = new JavaEnumeration(x)
  implicit def constantPredicate[A](value: Boolean): ToBool[A]            = if (value) ConstantTrue else ConstantFalse
  implicit def conforms[A] : (A <:< A)                                    = new conformance[A]
}
