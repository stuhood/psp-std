package psp
package dev

import std._
import com.squareup.{ okhttp => ok }

final class Url(val url: String) extends AnyVal {
  private def doGet(): ok.ResponseBody = {
    val req = new ok.Request.Builder url url build
    val resp = okClient newCall req
    resp.execute.body
  }
  def string(): String            = doGet.string
  def bytes(): Array[Byte]        = doGet.bytes
  def contentType(): ok.MediaType = doGet.contentType
  def size(): api.Size            = Size(doGet.contentLength)
}


