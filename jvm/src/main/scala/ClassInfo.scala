package psp
package std
package jvm

import scala.tools.nsc.io.{ Directory, Jar }
import psp.std.token.Keyword

sealed trait HasAttributes                                  { def attributes: Array[AttributeInfo]     }
sealed trait HasJvmFlags                                    { def flags: Flags                         }
sealed trait JvmInfo extends HasAttributes with HasJvmFlags { def name: JvmName ; def keyword: Keyword }

object ClassInfo {
  def fromBytes(bytes: Array[Byte]): ClassInfo = fromStream(new ByteArrayInputStream(bytes))
  def fromFile(file: jFile): ClassInfo         = fromStream(new BufferedInputStream(new FileInputStream(file)))
  def fromPath(p: Path): ClassInfo             = fromFile(p.toFile)
  def fromPath(p: String): ClassInfo           = fromPath(path(p))
  def fromStream(in: InputStream): ClassInfo   = new DataInputStream(in) |> (in => try fromDataInput(in) finally in.close())
  def fromDataInput(in: DataInput): ClassInfo  = new Builder(in) parse
}

trait HasIsEmpty[A] extends Any with api.Opt[A] {
  self: A =>
  def isEmpty: Boolean
  def get: A = self
}

final case class ClassInfo(
        name: JvmName,
       flags: Flags,
   superName: JvmName,
  interfaces: Array[JvmName],
      fields: Array[MemberInfo],
     methods: Array[MemberInfo],
  attributes: Array[AttributeInfo],
 poolEntries: Array[PoolEntry]
)
extends JvmInfo {
  def keyword = flags.classKeyword
}

// method_info or field_info {
//   u2 access_flags;
//   u2 name_index;
//   u2 descriptor_index;
//   u2 attributes_count;
//   attribute_info attributes[attributes_count];
// }
final case class MemberInfo(flags: Flags, ref: JvmRef, attributes: Array[AttributeInfo]) extends JvmInfo {
  def name    = ref.name
  def keyword = if (ref.isField) Keyword.Field else Keyword.Def
}

// package foo { class Foo { class Bar } }
//
// javap would say
//    Bar = class foo.Foo$Bar of class foo.Foo
// which is translated as
//   innerClass = foo.Foo$Bar
//   outerClass = foo.Foo
//    innerName = Bar
final case class InnerClassInfo(
   thisClass: JvmName,   // classfile which is being parsed
  innerClass: JvmName,   // the full name of the inner/nested class
  outerClass: JvmName,   // the full name of the outer class - must be a prefix of innerClass
   innerName: JvmName,   // the simple name of the inner class - should (?) be a suffix of innerClass
       flags: Flags      // flags
)

final case class MethodParameter(name: JvmName, flags: Flags)
