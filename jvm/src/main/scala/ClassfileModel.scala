package psp
package std
package jvm

import pool._
import JvmConstants._
import scala.collection.{ mutable, immutable }

final case class JvmVersion(minorVersion: Int, majorVersion: Int)

trait ClassfileModel {
  type Result
  type Entry
  type MemberInfo
  type AttributeInfo
  type InnerClassInfo

  protected implicit def EntryTag: CTag[Entry]
  protected implicit def MemberInfoTag: CTag[MemberInfo]
  protected implicit def AttributeInfoTag: CTag[AttributeInfo]
  protected implicit def InnerClassInfoTag: CTag[InnerClassInfo]

  def readConstantPoolEntry(): Entry
  def readInterface(): JvmName
  def readMember(): MemberInfo
  def readAttribute(): AttributeInfo
  def readInnerClass(): InnerClassInfo

  def refAt(idx: Int): JvmRef
  def nameAt(idx: Int): Lazy[String]
  def stringAt(idx: Int): String

  def createInfo(
     version: JvmVersion,
     entries: Array[Entry],
       flags: Flags,
        name: String,
   superName: JvmName,
  interfaces: Array[JvmName],
      fields: Array[MemberInfo],
     methods: Array[MemberInfo],
  attributes: Array[AttributeInfo]
           ): Result
}

abstract class StreamingClassfileModel extends ClassfileModel {
  protected[this] val in: DataInput
  private[this] var name: String = _
  private[this] var entries: Array[PoolEntry] = _
  private[this] val refCache = mutable.Map[Int, JvmRef]()
  private[this] def entryAt(idx: Int): PoolEntry = if (entries eq null) null else entries(idx)

  private def getIndexHash(x: Ref_Info): Int = (x.class_index << 16) | x.name_and_type_index

  private def fail(expected: String, idx: Int, actual: Any): Nothing = abort(s"No $expected at index $idx: $actual")

  private def getRef(index_hash: Int): JvmRef = {
    refCache.getOrElseUpdate(index_hash, {
      val classIdx  = (index_hash >> 16)
      val memberIdx = (index_hash & 0xFFFF)
      JvmRef(jvmNameAt(classIdx), jvmNameAt(memberIdx), descriptorAt(memberIdx))
    })
  }

  def pool = if (entries eq null) sciVector() else entries.toScalaVector

  type Entry = PoolEntry

  def refAt(idx: Int): JvmRef = entryAt(idx) match {
    case x: Ref_Info => getRef(getIndexHash(x))
    case x           => fail("ref", idx, x)
  }
  def jvmNameAt(idx: Int): Lazy[JvmName] = nameAt(idx) map (s => JvmName(s))
  def nameAt(idx: Int): Lazy[String] = entryAt(idx) match {
    case null         => Lazy("")
    case x: Name_Info => Lazy(stringAt(x.name_index).replace('/', '.'))
    case Utf8_info(s) => Lazy({ (new Throwable).printStackTrace ; s"nameAt($idx)" }) //   == $s" })
    // Lazy({ println(s"nameAt($idx) == $s") ; s"nameAt($idx) == $s" })
    case x            => fail("name", idx, x)
  }
  def descriptorAt(idx: Int): Descriptor = entryAt(idx) match {
    case NameAndType_info(nindex, dindex) => Descriptor(stringAt(dindex))
    case x                                => fail("descriptor", idx, x)
  }
  def stringAt(idx: Int): String = entryAt(idx) match {
    case Utf8_info(value) => value
    case _                => ""
  }
  def floatAt(idx: Int): Float = entryAt(idx) match {
    case Float_info(value) => value
    case x                 => fail("float", idx, x)
  }
  def doubleAt(idx: Int): Double = entryAt(idx) match {
    case Double_info(value) => value
    case x                  => fail("double", idx, x)
  }
  def longAt(idx: Int): Long = entryAt(idx) match {
    case Long_info(value) => value
    case x                => fail("long", idx, x)
  }
  def integerAt(idx: Int): Int = entryAt(idx) match {
    case Integer_info(value) => value
    case x                   => fail("int", idx, x)
  }
  def constantAt(idx: Int): Any = entryAt(idx) match {
    case null                => ""
    case Integer_info(value) => value
    case Long_info(value)    => value
    case Float_info(value)   => value
    case Double_info(value)  => value
    case String_info(sindex) => stringAt(sindex)
    case x                   => fail("constant", idx, x)
  }
  protected def u4 = in.readInt
  protected def u2 = in.readUnsignedShort.toChar
  protected def u1 = in.readUnsignedByte

  // The constant_pool table is indexed from 1 to constant_pool_count−1.
  protected def readConstantPool(): Array[Entry] = {
    val count = u2
    val entries = new Array[Entry](count)
    var i = 1
    while (i < count) {
      val entry = readConstantPoolEntry()
      entries(i) = entry
      i += entry.width
    }
    entries
  }
  protected def readInterfaces(): Array[JvmName] = {
    val count = u2
    val interfaces = new Array[JvmName](count)
    var i = 0
    while (i < count) {
      interfaces(i) = readInterface()
      i += 1
    }
    interfaces
  }
  protected def readMembers() = {
    val count = u2
    val arr = new Array[MemberInfo](count)
    var i = 0
    while (i < count) {
      arr(i) = readMember()
      i += 1
    }
    arr
  }
  protected def readAttributes(): Array[AttributeInfo] = {
    val count = u2
    val arr = new Array[AttributeInfo](count)
    var i = 0
    while (i < count) {
      arr(i) = readAttribute()
      i += 1
    }
    arr
  }
  protected def readInnerClasses() = {
    val count = u2
    val arr = new Array[InnerClassInfo](count)
    var i = 0
    while (i < count) {
      arr(i) = readInnerClass()
      i += 1
    }
    arr
  }
  protected def thisClass = name

  def parse() = {
    assert(u4 == JAVA_MAGIC, s"Bad magic number: expected $JAVA_MAGIC, found $u4")
    val version    = JvmVersion(u2, u2)
    this.entries   = readConstantPool()
    val flags      = Flags(u2.toChar)
    this.name      = nameAt(u2)
    val superName  = jvmNameAt(u2)
    val interfaces = readInterfaces()
    val fields     = readMembers()
    val methods    = readMembers()
    val attributes = readAttributes()

    createInfo(version, entries, flags, name, superName, interfaces, fields, methods, attributes)
  }
}

abstract class ScalacClassfileModel extends StreamingClassfileModel {
  type Result         = jvm.ClassInfo
  type MemberInfo     = jvm.MemberInfo
  type AttributeInfo  = jvm.AttributeInfo
  type InnerClassInfo = jvm.InnerClassInfo

  protected def EntryTag: CTag[Entry]                   = implicitly[CTag[Entry]]
  protected def MemberInfoTag: CTag[MemberInfo]         = implicitly[CTag[MemberInfo]]
  protected def AttributeInfoTag: CTag[AttributeInfo]   = implicitly[CTag[AttributeInfo]]
  protected def InnerClassInfoTag: CTag[InnerClassInfo] = implicitly[CTag[InnerClassInfo]]

  def readConstantPoolEntry(): PoolEntry
  def readInterface(): JvmName
  def readMember(): MemberInfo
  def readAttribute(): AttributeInfo
  def readInnerClass(): InnerClassInfo

  def createInfo(
    version: JvmVersion,
    entries: Array[PoolEntry],
    flags: Flags,
    name: String,
    superName: JvmName,
    interfaces: Array[JvmName],
    fields: Array[MemberInfo],
    methods: Array[MemberInfo],
    attributes: Array[AttributeInfo]
  ): ClassInfo = new ClassInfo(name, flags, superName, interfaces, fields, methods, attributes, entries)
}
