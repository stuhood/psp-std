package psp
package std
package jvm

import StdShow._, StdEq._
import scala.collection.:+

// Type[]  getBounds()
// Returns an array of Type objects representing the upper bound(s) of this type variable.
// D getGenericDeclaration()
// Returns the GenericDeclaration object representing the generic declaration declared this type variable.
// String  getName()
// Returns the name of this type variable, as it occurs in the source code.

trait jGenericDeclarationOps[D <: jGenericDeclaration] extends Any {
  def declaration: D
  def typeParams: Vec[jTypeVar] = declaration.getTypeParameters.toVec
  def isGeneric                 = typeParams.nonEmpty
}

final class jTypeVariableOps[D <: jGenericDeclaration](val x: jTypeVariable[D]) extends AnyVal with jGenericDeclarationOps[D] {
  def bounds      = x.getBounds filterNot (_ eq JavaObject)
  def name        = x.getName
  def declaration = x.getGenericDeclaration
}

final class jParameterizedTypeOps(val tp: jParameterizedType) extends AnyVal {
  def instantiated = constructor.typeParams zip args toMap
  def owner        = tp.getOwnerType
  def constructor  = tp.getRawType
  def args         = tp.getActualTypeArguments
}

final class jTypeOps(val declaration: jType) extends AnyVal with jTypeAndClassOps {
  def typeParams: Vec[jTypeVar] = declaration match {
    case x: jGenericDeclaration => x.getTypeParameters.toVec
    case _                      => emptyValue
  }
}

trait jTypeAndClassOps extends Any {
  def declaration: jType
  def typeParams: Vec[jTypeVar]

  def simpleName: String = declaration match {
    case c: jClass             => c.getName.dottedSegments.last
    case x: jTypeVar           => x.getName
    case x: jWildcardType      => "_"
    case x: jParameterizedType => x.constructor.simpleName
    case x: jGenericArrayType  => "Array"
  }

  // def mapOver(f: jType => jType): jType = x match {
  //   case jAppliedArray(elem) =>
  // }

  def ancestors: Array[jType] = declaration match {
    case x: jClass => (x.parents.m ++ x.parents.m.flatMap(_.ancestors)).byEquals.distinct.toArray
    case _         => Array()
  }

  //  = x match {
  //   case x: jGenericDeclaration => x.getTypeParameters.seq
  //   case _                      => sciSeq()
  // }

  def scalaString = refString
  def defString: String = declaration match {
    case jTypeParam(name, lower, scSeq()) => name
    case jTypeParam(name, lower, upper)   => show"$name <: ${upper.m.joinParents}"
    case _                                => refString
  }
  def refString: String = declaration match {
    case jSpecialType(name)            => name
    case jAliased(alias)               => alias
    case jTypeParam(name, _, _)        => name
    case jTypeArg(Nil, Nil)            => "_"
    case jTypeArg(lower, Nil)          => show"_ >: ${lower.m.joinParents}"
    case jTypeArg(Nil, upper)          => show"_ <: ${upper.m.joinParents}"
    case jTypeArg(lower, upper)        => show"_ >: ${lower.m.joinParents} <: ${upper.m.joinParents}"
    case jInfixType(lhs, name, rhs)    => show"$lhs $name $rhs"
    case sPartialFunction(scSeq(t, r)) => show"$t ?=> $r"
    case sFunction(scSeq(p) :+ last)   => show"$p => $last"
    case sFunction(init :+ last)       => init.m.inParens ~ " => ".s ~ last render
    case sTuple(xs)                    => xs.m.inParens
    case jAppliedArray(elem)           => show"Array[$elem]"
    case jApplied(tcon, Nil)           => show"$tcon"
    case jApplied(tcon, args)          => show"$tcon[${args mk_s ", "}]"
    case x: jClass                 =>
      val prefix    = x.shortPackage mapNonEmpty (_ + ".")
      val enclosing = x.owner.fold("")(o => show"$o.")
      val simple    = x.scalaSimpleName
      show"$prefix$enclosing$simple"
  }
}


//     case _                   =>
//       val prefix    = shortPackage mapNonEmpty (_ + ".")
//       val enclosing = owner.fold("")(o => show"$o.")
//       show"$prefix$enclosing$scalaSimpleName"
//   }

//   def scalaString: String = x match {
//     case jSpecialType(name)      => name
//     case sFunction(Seq(x1, x2))  => show"$x1 => $x2"
//     case sFunction(init :+ last) => (each(init).inParens ~ " => " ~ last).to_s
//     case sTuple(xs)              => "(" + each(xs).inParens + ")"
//     case jApplied(tcon, Seq())   => tcon.to_s
//     case jApplied(tcon, args)    => show"$tcon[${args.joinComma}]"
//     case jGenericArray(elem)     => show"Array[$elem]"
//     case x: jClass               => x.shortName
//   }

//   // implicit def jTypeShow: Show[jType] = Show {
//   //   case null                  => "<null>"
//   //   case _: jWildcardType      => "_"
//   //   case x: jTypeVariable[_]   => x.getName
//   //   case x: jParameterizedType => x.constructor.to_s + x.args.m.optBrackets
//   //   case x: jGenericArrayType  => show"Array[${x.getGenericComponentType}]"
//   //   case x: jClass             => x.shortName
//   // }
//   // implicit def jMethodShow: Show[jMethod] = Show { m =>
//   //   val ts = m.typeParams.m.optBrackets
//   //   val ps = m.paramTypes mapWithNth ((nth, tp) => show"p$nth: $tp") joinComma
//   //   val rs = m.returnType
//   //   // m stripPackage
//   //   show"def ${m.name}$ts($ps): $rs"
//   // }
// }

final class jMethodOps(val m: jMethod) extends AnyVal with jGenericDeclarationOps[jMethod] {
  import java.lang.reflect.{ Modifier => M }
  private def mods = m.getModifiers

  def isPublic     = M isPublic mods
  def isProtected  = M isProtected mods
  def isStatic     = M isStatic mods
  def isFinal      = M isFinal mods
  def isNative     = M isNative mods
  def isAbstract   = M isAbstract mods

  def name             = decodeName(m.getName)
  def declaration      = m
  def isSpecialized    = m.getName contains "$mc"
  def paramTypes       = m.getGenericParameterTypes.toVec
  def returnType       = m.getGenericReturnType
  def enclosingClass   = m.getDeclaringClass
  def enclosingPackage = enclosingClass.getPackage
  def enclosingTag     = CTag(enclosingClass)

  // def paramNames: Vec[String] = paramNamesOf(m)

  // def paramNameMap[A: CTag](): sciMap[jMethod, sciVector[String]] =
  //   methodSourceMap[A] mapValues (_.getParameters.asScala.toVector.map(_.getName.toString))


  def scalaString: String = { ""
    // val ts = typeParams.map(_.defString).optBrackets.render
    // val ps = zip2(paramNames, paramTypes) map ((name, tp) => show"$name: $tp") mkString ", "
    // val rs = returnType.refString
    // show"def $name$ts($ps): $rs"
  }

  def modifierString = {
    val sb = new StringBuilder
    // if (isPublic) sb append "public "
    if (isProtected) sb append "protected "
    // if (isStatic) sb append "static "
    if (isFinal) sb append "final "
    // if (isNative) sb append "native "
    // if (isAbstract) sb append "abstract "
    sb.toString
  }
}

final class jClassOps[A](val c: Class[A]) extends AnyVal with jGenericDeclarationOps[Class[A]] with jTypeAndClassOps {
  def x = c
  def declaration = c

  // private def makeScalaString(): String = c match {
  //   case sSpecial(name)            => name
  //   case sFunction(Seq(p) :+ last) => show"$p => $last"
  //   case sFunction(init :+ last)   => (each(init).inParens ~ " => " ~ last).to_s
  //   case jPrimitive(c)             => c.scalaSimpleName
  //   case _ if c.isArray            => show"Array[${c.getComponentType}]"
  //   case _                   =>
  //     val prefix    = shortPackage mapNonEmpty (_ + ".")
  //     val enclosing = owner.fold("")(o => show"$o.")
  //     show"$prefix$enclosing$scalaSimpleName"
  // }

  def owner: Option[jClass]    = Option(c.getEnclosingClass)
  def parents: Array[jType]    = Option(c.getGenericSuperclass).fold(interfaces)(_ +: interfaces toArray)
  def interfaces: Array[jType] = c.getGenericInterfaces
  def scalaSimpleName          = decodeName(simpleName).dollarSegments.last
  def isEncodedName            = decodeName(c.getName) != c.getName

  def internalName       = javaName.replace('.', '/')
  def javaName           = c.getName
  def classLoader        = c.getClassLoader
  def methods            = c.getMethods.toVec filterNot objectMethods
  def bridgeMethods      = methods filter (_.isBridge)
  def specializedMethods = methods filter (_.isSpecialized)
  def publicMethods      = methods filter (x => x.isPublic && !x.isBridge && !x.isSpecialized)
  def staticMethods      = publicMethods filter (_.isStatic)
  def instanceMethods    = publicMethods filterNot (_.isStatic)

  // def scalaSimpleName = c match {
  //   case java.lang.Void.TYPE => "Unit"
  //   case ScalaNothingClass   => "Nothing"
  //   case ScalaNullClass      => "Null"
  //   case _ if c.isPrimitive  => simpleName.capitalize
  //   case _ if c.isArray      => show"Array[${c.getComponentType}]"
  //   case _                   => decodeName(simpleName).dollarSegments.last
  // }

  // Short decoded class name.
  def shortName: String = scalaSimpleName
  // (c: jType).scalaString

  def apply[A: CTag] : jAppliedType                      = new jAppliedType(c, Array(classOf[A]))
  def apply[A1: CTag, A2: CTag] : jAppliedType           = new jAppliedType(c, Array(classOf[A1], classOf[A2]))
  def apply[A1: CTag, A2: CTag, A3: CTag] : jAppliedType = new jAppliedType(c, Array(classOf[A1], classOf[A2], classOf[A3]))
}
