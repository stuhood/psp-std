package psp
package std
package jvm

import api._
import scala.io.Codec.toUTF8

// field_info attributes: ConstantValue Synthetic Signature Deprecated RuntimeVisibleAnnotations RuntimeInvisibleAnnotations
// method_info attributes: Code Exceptions Synthetic Signature Deprecated RuntimeVisibleAnnotations
//   RuntimeInvisibleAnnotations RuntimeVisibleParameterAnnotations RuntimeInvisibleParameterAnnotations AnnotationDefault
sealed trait AttributeInfo extends Any
sealed trait EncodesScalaSig extends Any { def bytes(): Array[Byte] }
sealed trait ElementValue extends Any { def tag: Byte }

sealed trait JvmAnnotation {
  def tpe: JvmName
  def args: LazyPairs
}
sealed trait ScalaSigAttribute extends AttributeInfo with EncodesScalaSig
sealed trait ScalaSigJvmAnnotation extends JvmAnnotation with EncodesScalaSig
final case class ElementValuePair(name: String, value: ElementValue) { override def toString = s"$name = $value" }

sealed trait SignatureOrDescriptor                                                                extends Any
sealed trait Descriptor                                                                           extends Any {   def isMethod = false }
sealed trait TypeDescriptor                                                                       extends Any with Descriptor
final case class Tag(tag: Char)                                                                   extends AnyVal with TypeDescriptor
final case class ClassDescriptor(string: String)                                                  extends TypeDescriptor
final case class ArrayDescriptor(elem: TypeDescriptor, dimensions: Int)                           extends TypeDescriptor
final case class MethodDescriptor(paramTypes: Direct[TypeDescriptor], returnType: TypeDescriptor) extends Descriptor
final case class JvmSignature(descriptor: String)                                                 extends AnyVal with SignatureOrDescriptor
final case object NoDescriptor                                                                    extends Descriptor

object Descriptor {
  def apply(s: String): Descriptor = new ClassDescriptor(s) // XXX
}

final case class JvmRef(className: JvmName, name: JvmName, descriptor: Descriptor)
final case class JvmCall(from: JvmRef, to: JvmRef)
final case class JvmDef(
   superName: JvmName,
  interfaces: Array[String],
      fields: Array[MemberInfo],
     methods: Array[MemberInfo]
)
final case class JvmName(name: String) extends AnyVal with HasIsEmpty[JvmName] {
  def isEmpty = name == ""
}
object JvmName {
  implicit def ShowJvmName: Show[JvmName] = Show[JvmName](_.decoded)
}


// 4.7.16.1 The element_value structure
// The element_value structure is a discriminated union representing the value of an element-value pair. It has the following format:
//    element_value {
//        u1 tag;
//        union {
//            u2 const_value_index;
//            {   u2 type_name_index;
//                u2 const_name_index;
//            } enum_const_value;
//            u2 class_info_index;
//            annotation annotation_value;
//            {   u2            num_values;
//                element_value values[num_values];
//            } array_value;
//        } value;
// }

package attr {
  final case object Scala                                                       extends AttributeInfo
  final case object Bridge                                                      extends AttributeInfo
  final case object Deprecated                                                  extends AttributeInfo
  final case object Synthetic                                                   extends AttributeInfo
  final case class Generic(name: String, value: Bytes)                          extends AttributeInfo
  final case class Signature(value: String)                                     extends AttributeInfo
  final case class InnerClasses(value: Array[InnerClassInfo])                   extends AttributeInfo
  final case class ConstantValue(value: Any)                                    extends AttributeInfo
  final case class Exceptions(value: Array[String])                             extends AttributeInfo
  final case class AnnotationDefault(value: ElementValue)                       extends AttributeInfo
  final case class Code(value: jvm.Code)                                        extends AttributeInfo
  final case class RuntimeVisibleAnnotations(value: Array[Lazy[JvmAnnotation]]) extends AttributeInfo
  final case class EnclosingMethod(enclClass: JvmName, enclMethod: JvmName)     extends AttributeInfo
  final case class MethodParameters(value: Array[MethodParameter])              extends AttributeInfo
  final case class SourceFile(value: String)                                    extends AttributeInfo
  final case class ScalaSignature(bytes: Bytes)                                 extends ScalaSigAttribute
  final case class ScalaSig(bytes: Bytes)                                       extends ScalaSigAttribute

  final case class ConstantInteger(value: Int, tag: Byte)                 extends ElementValue // BCISZ
  final case class ConstantFloat(value: Float)                            extends ElementValue { def tag = 'F'.toByte }
  final case class ConstantDouble(value: Double)                          extends ElementValue { def tag = 'D'.toByte }
  final case class ConstantLong(value: Long)                              extends ElementValue { def tag = 'J'.toByte }
  final case class StringConstValue(value: String)                        extends ElementValue { def tag = 's'.toByte }
  final case class EnumConstValue(enumType: String, enumName: String)     extends ElementValue { def tag = 'e'.toByte }
  final case class ClassInfoValue(className: String)                      extends ElementValue { def tag = 'c'.toByte }
  final case class AnnotationNestedValue(nestedAnnotation: JvmAnnotation) extends ElementValue { def tag = '@'.toByte }
  final case class AnnotationArrayValue(arrayValues: Array[ElementValue]) extends ElementValue { def tag = LBracket.toByte }

  final case class MarkerAnnotation(tpe: JvmName) extends JvmAnnotation { val args = new LazyPairs(0) }
  final case class AnnotationWithArgs(tpe: JvmName, args: LazyPairs) extends JvmAnnotation
  final case class ScalaSigAnnotation(elem: StringConstValue) extends ScalaSigJvmAnnotation {
    def tpe        = ScalaSigAnnotationName
    lazy val args  = lazyBytes(elem)
    lazy val bytes = toUTF8(elem.value)
  }
  final case class ScalaLongSigAnnotation(elem: AnnotationArrayValue) extends ScalaSigJvmAnnotation {
    def tpe        = ScalaLongSigAnnotationName
    lazy val args  = lazyBytes(elem)
    lazy val bytes: Bytes = elem.arrayValues.m gather { case StringConstValue(x) => toUTF8(x) } force
  }
}
