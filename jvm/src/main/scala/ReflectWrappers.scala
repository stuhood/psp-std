package psp
package std
package jvm

import attr._
// import javax.tools._
import scala.reflect.internal.pickling.ByteCodecs

object PathClassInfo extends pio.PathCache(ClassInfo.fromPath)

final class JvmInfoOps(val info: JvmInfo) extends AnyVal {
  import info._

  def decodedName = name.decoded
  def packageName = name.packageName
  def simpleName  = name.simpleName
  def findBytes: Option[Bytes] = (
           (info.attributes first { case x: ScalaSignature => x.bytes })
    orElse (info.annotations first { case Lazy(x: ScalaSigJvmAnnotation) => x.bytes })
  )
  def scalaSigBytes: Array[Byte] = {
    val bytes = findBytes | Array[Byte]()
    bytes take (ByteCodecs decode bytes).size force
  }
  def carriesScalaSignature = scalaSigBytes.length > 0
  def needsScalaSignature   = attributes contains attr.Scala
  def isScalaDefined        = needsScalaSignature || carriesScalaSignature
  def isJavaDefined         = !isScalaDefined
}

final class jAppliedType(constructor: jType, args: Array[jType]) extends jParameterizedType {
  def instantiated           = constructor.typeParams zip getActualTypeArguments toMap
  def getRawType             = constructor
  def getOwnerType           = constructor match {
    case x: jClass => x.getEnclosingClass
    case _         => null
  }
  def getActualTypeArguments = args
  override def toString      = this.to_s
}

object jApplied {
  def unapply(x: jType): Option[(jType, scSeq[jType])] = x match {
    case x: jParameterizedType if x.args.nonEmpty => Some(x.constructor -> x.args.seq)
    case _                                        => None
  }
}
object jAppliedArray {
  // def apply(elem: jType) = elem match {
  //   case

  def unapply(x: jType): Option[jType] = x match {
    case x: jGenericArrayType   => Some(x.getGenericComponentType)
    case x: jClass if x.isArray => Some(x.getComponentType)
    case _                      => None
  }
}
object jInfixType {
  def unapply(x: jType): Option[(jType, String, jType)] = x match {
    case jApplied(jOperatorName(name), scSeq(lhs, rhs)) => Some((lhs, name, rhs))
    case _                                            => None
  }
}
object jAliased {
  def unapply(x: jClass): Option[String] = x.getName match {
    case "java.util.Iterator"                     => Some("jIterator")
    case "java.lang.Iterable"                     => Some("jIterable")
    case "java.lang.String"                       => Some("String")
    case "java.lang.Class"                        => Some("jClass")
    case "java.lang.reflect.Type"                 => Some("jType")
    case "java.lang.reflect.Method"               => Some("jMethod")
    case "java.lang.reflect.TypeVariable"         => Some("jTypeVariable")
    case "java.lang.annotation.Annotation"        => Some("jAnnotation")
    case "scala.math.Ordering"                    => Some("Ordering")
    case "scala.math.Numeric"                     => Some("Numeric")
    case "scala.Option"                           => Some("Option")
    case "scala.Some"                             => Some("Some")
    case "scala.reflect.CTag"                 => Some("CTag")
    case "scala.reflect.Manifest"                 => Some("Manifest")
    case "scala.collection.mutable.StringBuilder" => Some("StringBuilder")
    case "scala.collection.mutable.Builder"       => Some("Builder")
    case "scala.collection.immutable.Vector"      => Some("sciVector")
    case "scala.collection.immutable.List"        => Some("sciList")
    case "scala.collection.generic.CanBuildFrom"  => Some("CBF")
    case "scala.collection.generic.FilterMonadic" => Some("FilterMonadic")
    case "scala.collection.TraversableOnce"       => Some("TOnce")
    case "scala.collection.Traversable"           => Some("Traversable")
    case "scala.collection.Iterable"              => Some("Iterable")
    case "scala.collection.Seq"                   => Some("Seq")
    case "scala.collection.GenTraversableOnce"    => Some("GTOnce")
    case "scala.collection.GenTraversable"        => Some("sCollection")
    case "scala.collection.GenIterable"           => Some("GenIterable")
    case "scala.collection.GenMap"                => Some("GenMap")
    case "scala.collection.GenSeq"                => Some("GenSeq")
    case "scala.collection.GenSet"                => Some("GenSet")
    case "scala.collection.Iterator"              => Some("sIterator")
    case _                                        => None
  }
}
object jShortPackage {
  def unapply(x: jPackage): Option[String] = if (x eq null) None else x.getName match {
    case "psp.std.api"                => Some("api")
    case "psp.std"                    => Some("std")
    case "java.lang"                  => Some("jl")
    case "java.lang.reflect"          => Some("jlr")
    case "scala.collection.immutable" => Some("sci")
    case "scala.collection.mutable"   => Some("scm")
    case "scala.collection.generic"   => Some("scg")
    case "scala.collection"           => Some("sc")
    case "java.nio.file"              => Some("jnf")
    case "java.nio.file.attribute"    => Some("jnfa")
    case _                            => None
  }
}
object jTypeParam {
  def unapply(x: jType): Option[(String, scSeq[jType], scSeq[jType])] = x match {
    case x: jTypeVariable[_] => Some((x.getName, Nil, x.bounds.seq))
    case _                   => None
  }
}
object jTypeArg {
  def unapply(x: jType): Option[(scSeq[jType], scSeq[jType])] = x match {
    case x: jWildcardType => Some((x.getLowerBounds.seq, x.getUpperBounds.m.byEquals without JavaObject seq))
    case _                => None
  }
}

object jClassDefinition {
  final case class Flat(pkg: jPackage, name: String, tparams: scSeq[jTypeVar], parents: scSeq[jType])
    extends scala.Product4[jPackage, String, scSeq[jTypeVar], scSeq[jType]] {
      def _1 = pkg
      def _2 = name
      def _3 = tparams
      def _4 = parents
    }

  def unapply(x: jType): Option[Flat] = x match {
    case x: jClass => Some(Flat(x.getPackage, x.simpleName, x.typeParams.seq, x.parents.seq))
    case _         => None
  }
}

object jSpecialType {
  def unapply(x: jType): Option[String] = x match {
    case null          => Some("<null>")
    case JavaVoid      => Some("Unit")
    case JavaObject    => Some("Any")
    case ScalaNothing  => Some("Nothing")
    case ScalaNull     => Some("Null")
    case jPrimitive(c) => Some(c.getName.capitalize)
    case _             => None
  }
}
object jPrimitive {
  def unapply(x: jType): Option[jClass] = x match {
    case x: jClass if x.isPrimitive => Some(x)
    case _                          => None
  }
}

object jOperatorName {
  def unapply(x: jType): Option[String] = x match {
    case x: jClass if x.isEncodedName => Some(x.scalaSimpleName)
    case x: jParameterizedType        => unapply(x.constructor)
    case _                            => None
  }
}
object jClassName {
  def unapply(x: jType): Option[String] = x match {
    case x: jClass             => Some(x.getName)
    case x: jParameterizedType => unapply(x.constructor)
    case _                     => None
  }
}
object jReturnType { def unapply(x: jMethod): scala.Some[jType] = Some(x.returnType) }
object jParamTypes { def unapply(x: jMethod): scala.Some[scSeq[jType]] = Some(x.paramTypes.seq) }

final case class jTypeArgExtractor(Arity: Regex) {
  def unapply(x: jType): Option[scSeq[jType]] = x match {
    case jApplied(jClassName(Arity(n)), args) => Some(args)
    case _                                    => None
  }
}
