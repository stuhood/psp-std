package psp
package std
package pio

import api._

/** Memoization for the results of Path => A functions, where
 *  it's assumed the path translates into an actual file. The file
 *  modification time determines whether the cached value needs
 *  to be recalculated.
 */
class PathCache[A](f: Path => A) extends (Path => A) {
  private[this] val timestamps = mutableMap[Path, FileTime]() withDefaultValue FileTime.empty
  private[this] val content    = mutableMap[Path, A]()
  private def timestampOk(path: Path) = path.lastModified == timestamps(path)
  private def updateCache(path: Path): A = {
    timestamps(path) = path.lastModified
    f(path) doto (content(path) = _)
  }
  def clear(): Unit = {
    timestamps.clear()
    content.clear()
  }
  def apply(path: Path): A = content get path match {
    case Some(c) if timestampOk(path) => c
    case _                            => updateCache(path)
  }
}

/** Standard memoizing objects for the given types.
 */
object PathBytes extends PathCache[Array[Byte]](Files readAllBytes _)
object PathChars extends PathCache[Array[Char]](path => utf8(PathBytes(path)).chars)
object PathLines extends PathCache[View[String]](Files readAllLines _ m)
object PathSlurp extends PathCache[String](path => utf8(PathBytes(path)).to_s)
object PathJars  extends PathCache[Jar](path => Jar(path))
