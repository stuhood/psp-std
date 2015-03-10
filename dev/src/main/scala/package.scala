package psp

import std._
import com.squareup.{ okhttp => ok }

package object dev {
  def readAs[A](implicit z: Read[A]): String => A = z.read
  def asExpected[A](body: Any): A                 = body.castTo[A]

  val cacheSize     = 1024L * 1024L * 100L
  val cacheDir      = userHome.toFile / ".pspcache"
  val cacheInstance = new ok.Cache(cacheDir, cacheSize)
  val okClient      = new ok.OkHttpClient doto (_ setCache cacheInstance)

  implicit class JavaFileOps(val f: jFile) extends AnyVal {
    def /(segment: String): jFile = new jFile(f, segment)
  }
}
