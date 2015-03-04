// package psp
// package std
// package jvm

// import api._

// sealed trait SignatureOrDescriptor                                                                extends Any
// sealed trait Descriptor                                                                           extends Any {   def isMethod = false }
// sealed trait TypeDescriptor                                                                       extends Any with Descriptor
// final case class Tag(tag: Char)                                                                   extends AnyVal with TypeDescriptor
// final case class ClassDescriptor(string: String)                                                  extends TypeDescriptor
// final case class ArrayDescriptor(elem: TypeDescriptor, dimensions: Int)                           extends TypeDescriptor
// final case class MethodDescriptor(paramTypes: Direct[TypeDescriptor], returnType: TypeDescriptor) extends Descriptor
// final case class JvmSignature(descriptor: String)                                                 extends AnyVal with SignatureOrDescriptor
// final case object NoDescriptor                                                                    extends Descriptor
// object Descriptor { def apply(s: String): Descriptor = ??? }

// final case class JvmRef(className: JvmName, name: JvmName, descriptor: Descriptor)
// final case class JvmCall(from: JvmRef, to: JvmRef)
// final case class JvmDef(
//    superName: JvmName,
//   interfaces: Array[String],
//       fields: Array[MemberInfo],
//      methods: Array[MemberInfo]
// )
// final case class JvmName(name: String) extends AnyVal with HasIsEmpty[JvmName] {
//   def isEmpty = name == ""
// }
// object JvmName {
//   implicit def ShowJvmName: Show[JvmName] = Show[JvmName](_.decoded)
// }
