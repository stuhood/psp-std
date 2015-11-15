package psp
package std

import java.nio.{ file => jnf }
import java.nio.file.{ attribute => jnfa }

object Paths {
  def get(path: String): jnf.Path = jnf.Paths get path
}

object FileTime {
  val NoFileTime                        = this fromMillis MinLong
  def empty: FileTime                   = NoFileTime
  def fromMillis(value: Long): FileTime = jnfa.FileTime fromMillis value
}

object Files {
  def readAllBytes(path: jnf.Path): Array[Byte]                = jnf.Files.readAllBytes(path)
  def readAllLines(path: jnf.Path, cs: Charset): jList[String] = jnf.Files.readAllLines(path, cs)
  def readAllLines(path: jnf.Path): jList[String]              = readAllLines(path, defaultCharset)
}
