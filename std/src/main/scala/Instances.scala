package psp
package impl

import api._, std._

trait PrimitiveInstances {
  implicit def boolOrder: Order[Bool]   = orderBy[Bool](x => if (x) 1 else 0)
  implicit def byteOrder: Order[Byte]   = Order.fromInt(_ - _)
  implicit def charOrder: Order[Char]   = Order.fromInt[Char](_ - _)
  implicit def shortOrder: Order[Short] = Order.fromInt[Short](_ - _)
  implicit def intOrder: Order[Int]     = Order.fromInt[Int](_ - _)
  implicit def longOrder: Order[Long]   = Order.fromLong[Long](_ - _)
  implicit def unitOrder: Order[Unit]   = Order.fromInt[Unit]((x, y) => 0)
}

trait AlgebraInstances {
  implicit def identityAlgebra : BooleanAlgebra[Boolean]           = Algebras.Identity
  implicit def predicateAlgebra[A] : BooleanAlgebra[ToBool[A]]     = new Algebras.ToBool[A]
  implicit def intensionalSetAlgebra[A] : BooleanAlgebra[InSet[A]] = new Algebras.InSetAlgebra[A]
}

trait OrderInstancesLow {
  // If this is written in the obvious way, i.e.
  //
  //   implicit def comparableOrder[A <: Comparable[A]] : Order[A]
  //
  // Then it isn't found for infix operations on Comparables. We also can't call
  // Order.natural[A]() because it has that bound, despite the implicit witness.
  implicit def comparableOrder[A](implicit ev: A <:< Comparable[A]): Order[A] = Order.fromInt[A](_ compareTo _)
}

trait OrderInstances extends OrderInstancesLow {
  // Some unfortunate rocket dentistry necessary here.
  // This doesn't work because scala comes up with "Any" due to the fbound.
  // implicit def enumOrder[A <: jEnum[A]]: Order[A] = Order.fromInt[A](_.ordinal - _.ordinal)
  //
  // This one doesn't work if it's A <:< jEnum[A], but jEnum[_] is just enough to get what we need.
  implicit def enumOrder[A](implicit ev: A <:< jEnum[_]): Order[A] = orderBy[A](_.ordinal) // Order.fromInt[A](_.ordinal - _.ordinal)

  implicit def indexOrder: Order[Index]                                     = orderBy[Index](_.get)
  implicit def preciseOrder[A <: Precise]: Order[A]                         = orderBy[Precise](_.longValue)
  implicit def stringOrder: Order[String]                                   = Order.fromLong[String](_ compareTo _)
  implicit def tuple2Order[A: Order, B: Order] : Order[(A, B)]              = orderBy[(A, B)](fst) | snd
  implicit def tuple3Order[A: Order, B: Order, C: Order] : Order[(A, B, C)] = orderBy[(A, B, C)](_._1) | (_._2) | (_._3)
}

trait EmptyInstances0 {
  implicit def emptyCanBuild[R](implicit z: CanBuild[_, R]): Empty[R] = Empty(z().result)
}

trait EmptyInstances extends EmptyInstances0 {
  implicit def emptyBaseView[A, Repr] : Empty[BaseView[A, Repr]]    = Empty(new DirectView(Direct()))
  implicit def emptyBuilds[R](implicit z: Builds[_, R]): Empty[R]   = Empty(z build Each.empty)
  implicit def emptyExMap[K: Eq, V] : Empty[ExMap[K, V]]            = Empty(ExMap.empty[K, V])
  implicit def emptyInMap[K, V] : Empty[InMap[K, V]]                = Empty(InMap.empty[K, V])
  implicit def emptyExSet[A: Eq] : Empty[ExSet[A]]                  = Empty(exSet[A]())
  implicit def emptyInSet[A] : Empty[InSet[A]]                      = Empty(inSet[A](false))
  implicit def emptyJavaList[A] : Empty[jList[A]]                   = Empty(jList[A]())
  implicit def emptyJavaMap[K, V] : Empty[jMap[K, V]]               = Empty(jMap[K, V]())
  implicit def emptyJavaSet[A] : Empty[jSet[A]]                     = Empty(jSet[A]())
  implicit def emptyOption[A] : Empty[Option[A]]                    = Empty(None)
  implicit def emptyTuple[A: Empty, B: Empty]: Empty[(A, B)]        = Empty(emptyValue[A] -> emptyValue[B])
  implicit def emptyView[A] : Empty[View[A]]                        = Empty(exView())

  implicit def emptyFile: Empty[jFile]            = Empty(NoFile)
  implicit def emptyIndex: Empty[Index]           = Empty(NoIndex)
  implicit def emptyIndexRange: Empty[IndexRange] = Empty(indexRange(0, 0))
  implicit def emptyPath: Empty[Path]             = Empty(NoPath)
  implicit def emptyDoc: Empty[Doc]               = Empty(Doc.NoDoc)
  implicit def emptyString: Empty[String]         = Empty("")
}

trait EqInstances extends OrderInstances {
  implicit def jTypeEq: Hash[jType]           = inheritEq
  implicit def offsetEq: Hash[Offset]         = inheritEq
  implicit def pathEq: Eq[Path]               = eqBy[Path](_.any_s)
  implicit def policyClassEq: Hash[JavaClass] = inheritEq
  implicit def sizeEq: Eq[Size]               = Eq(Size.equiv)

  /** The throwableEq defined above conveniently conflicts with the actual
   *  implicit parameter to the method. W... T... F. On top of this the error
   *  message is simply "value === is not a member of Throwable".
   */
  implicit def tryEq[A](implicit z1: Eq[A], z2: Eq[Throwable]): Eq[Try[A]] = Eq {
    case (Success(x), Success(y)) => x === y
    case (Failure(x), Failure(y)) => x === y // would be x === y, but.
    case _                        => false
  }

  // Since Sets are created with their own notion of equality, you can't pass
  // an Eq instance. Map keys are also a set.
  implicit def arrayEq[A: Eq] : Eq[Array[A]]       = eqBy[Array[A]](_.toDirect)
  implicit def vectorEq[A: Eq] : Eq[Direct[A]]     = Eq(_ zip _ corresponds (_ === _))
  // implicit def exSetEq[A] : Eq[ExSet[A]]           = Eq(symmetrically[ExSet[A]](_ isSubsetOf _))
  // implicit def exMapEq[K, V: Eq] : Eq[ExMap[K, V]] = Eq((xs, ys) => xs.domain === ys.domain && (equalizer(xs.apply, ys.apply) forall xs.domain))

  // implicit def tuple2Eq[A: Eq, B: Eq] : Eq[(A, B)] = Eq[(A, B)]((l, r) => fst(l) === fst(r) && snd(l) === snd(r))

  def equalizer[A, B: Eq](f: A => B, g: A => B): FunctionEqualizer[A, B] = new FunctionEqualizer(f, g)
  def symmetrically[A](f: Relation[A]): Relation[A]                      = (x, y) => f(x, y) && f(y, x)
}

final class OrderBy[A] { def apply[B](f: A => B)(implicit z: Order[B]): Order[A] = z on f }
final class ShowBy[A]  { def apply[B](f: A => B)(implicit z: Show[B]): Show[A]   = z on f }
final class HashBy[A]  { def apply[B](f: A => B)(implicit z: Hash[B]): Hash[A]   = z on f }
final class EqBy[A]    { def apply[B](f: A => B)(implicit z: Eq[B]): Eq[A]       = z on f }
