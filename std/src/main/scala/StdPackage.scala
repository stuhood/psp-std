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
  implicit class DirectOps[A](val xs: Direct[A]) {
    def apply(i: Index): A = xs elemAt i
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
