package psp
package std
package jvm

import pool._
import JvmConstants._
import attr._

class Builder(protected[this] val in: DataInput) extends ScalacClassfileModel {
  outer =>

  def readCode()               = Code(u2, u2, readByteArray(u4), readExceptionTable(), readAttributes())(context = outer)
  def readDescriptor()         = Descriptor(stringAt(u2))
  def readInnerClass()         = InnerClassInfo(thisClass, jvmNameAt(u2), jvmNameAt(u2), JvmName(stringAt(u2)), Flags(u2.toChar))
  def readInterface()          = jvmNameAt(u2)
  def readMember()             = MemberInfo(Flags(u2.toChar), readRef(), readAttributes())
  def readRef()                = JvmRef(thisClass, stringAt(u2), readDescriptor())
  def readStringElementValue() = StringConstValue(stringAt(u2))

  def readByteArray(len: Int) = newArray[Byte](len) doto in.readFully
  def readConstantPoolEntry(): Entry = (u1: @switch) match {
    case CONSTANT_NoEntry            => NoEntry
    case CONSTANT_Utf8               => Utf8_info(in.readUTF)
    case CONSTANT_Integer            => Integer_info(in.readInt)
    case CONSTANT_Float              => Float_info(in.readFloat)
    case CONSTANT_Long               => Long_info(in.readLong)
    case CONSTANT_Double             => Double_info(in.readDouble)
    case CONSTANT_Class              => Class_info(u2)
    case CONSTANT_String             => String_info(u2)
    case CONSTANT_Fieldref           => Fieldref_info(u2, u2)
    case CONSTANT_Methodref          => Methodref_info(u2, u2)
    case CONSTANT_InterfaceMethodref => InterfaceMethodref_info(u2, u2)
    case CONSTANT_NameAndType        => NameAndType_info(u2, u2)
  }

  def readElementValue(): ElementValue = (u1: @switch) match {
    case 'D'      => ConstantDouble(doubleAt(u2))
    case 'F'      => ConstantFloat(floatAt(u2))
    case 'J'      => ConstantLong(longAt(u2))
    case 's'      => StringConstValue(stringAt(u2))
    case 'e'      => EnumConstValue(stringAt(u2), stringAt(u2))
    case 'c'      => ClassInfoValue(stringAt(u2))
    case '@'      => AnnotationNestedValue(readAnnotation)
    case LBracket => AnnotationArrayValue(readElementValueArray)
    case tag      => ConstantInteger(integerAt(u2), tag.toByte)
  }


  def readAnnotationArrayValue(): AnnotationArrayValue = {
    val tag = u1
    assert(tag == LBracket, tag)
    new AnnotationArrayValue(readElementValueArray)
  }
  def readElementValueArray(): Array[ElementValue] = {
    val count = u2
    val arr = new Array[ElementValue](count)
    var i = 0
    while (i < count) {
      arr(i) = readElementValue
      i += 1
    }
    arr
  }

  def readAnnotation(): Lazy[JvmAnnotation] = {
    val tpe   = jvmNameAt(u2)
    val count = u2
    tpe match {
      case Lazy(ScalaSigAnnotationName)     => Lazy(ScalaSigAnnotation(readStringElementValue()))
      case Lazy(ScalaLongSigAnnotationName) => Lazy(ScalaLongSigAnnotation(readAnnotationArrayValue()))
      case _ if count == 0                  => Lazy(MarkerAnnotation(tpe))
      case _                                =>
        val arr   = new Array[Lazy[ElementValuePair]](count)
        var i     = 0
        while (i < count) {
          val name_index = u2
          val elem = readElementValue
          arr(i) = Lazy(new ElementValuePair(nameAt(name_index), elem))
          i += 1
        }
        Lazy(AnnotationWithArgs(tpe, arr))
    }
  }

  def readAnnotations(): Array[Lazy[JvmAnnotation]] = {
    val count = u2
    val arr   = new Array[Lazy[JvmAnnotation]](count)
    var i     = 0
    while (i < count) {
      arr(i) = readAnnotation()
      i += 1
    }
    arr
  }

  def readExceptions(): Array[String] = {
    val count = u2
    val arr   = new Array[String](count)
    var i     = 0
    while (i < count) {
      arr(i) = nameAt(u2)
      i += 1
    }
    arr
  }

  def readExceptionTable(): Array[ExceptionHandler] = {
    val count = u2
    val arr   = new Array[ExceptionHandler](count)
    var i     = 0
    while (i < count) {
      arr(i) = new ExceptionHandler(u2, u2, u2, nameAt(u2))
      i += 1
    }
    arr
  }
  def readMethodParameters(): Array[MethodParameter] = {
    val count = u2
    val arr   = new Array[MethodParameter](count)
    var i     = 0
    while (i < count) {
      arr(i) = new MethodParameter(jvmNameAt(u2), Flags(u2))
      i += 1
    }
    arr
  }

  def readNameAndType(): JvmName = u2 match {
    case 0   => NoJvmName
    case idx => jvmNameAt(idx)
  }

  def readAttribute(): AttributeInfo = {
    val name = stringAt(u2)
    val len  = u4

    name match {
      case "AnnotationDefault"                     => attr.AnnotationDefault(readElementValue())
      case "Bridge"                                => attr.Bridge
      case "Code"                                  => attr.Code(readCode())
      case "ConstantValue"                         => attr.ConstantValue(constantAt(u2))
      case "Deprecated"                            => attr.Deprecated
      case "EnclosingMethod"                       => attr.EnclosingMethod(jvmNameAt(u2), readNameAndType())
      case "Exceptions"                            => attr.Exceptions(readExceptions())
      case "InnerClasses"                          => attr.InnerClasses(readInnerClasses())
      case "MethodParameters"                      => attr.MethodParameters(readMethodParameters())
      case "RuntimeVisibleAnnotations"             => attr.RuntimeVisibleAnnotations(readAnnotations())
      case "Scala"                                 => attr.Scala
      case "ScalaSignature"                        => attr.ScalaSignature(readByteArray(len))
      case "Signature"                             => attr.Signature(stringAt(u2))
      case "Synthetic"                             => attr.Synthetic
      case "ScalaSig"                              => attr.ScalaSig(readByteArray(len))
      case "SourceFile"                            => attr.SourceFile(stringAt(u2))
      // case "LineNumberTable"                    =>
      // case "LocalVariableTable"                 =>
      // case "LocalVariableTypeTable"             =>
      // case "RuntimeInvisibleAnnotations"        =>
      // case "RuntimeVisibleParameterAnnotations" =>
      case name                                    => attr.Generic(name, readByteArray(len))
    }
  }
}
