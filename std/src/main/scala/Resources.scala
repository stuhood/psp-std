package psp
package std

object Resources {
  def getResourceNames(cl: ClassLoader, p: Path): Array[String] = {
    val path = s"$p/"
    Option(cl getResource path) match {
      case Some(dirUrl) if dirUrl.getProtocol == "file" => new jFile(dirUrl.toURI).list
      case Some(dirUrl) if dirUrl.getProtocol == "jar"  => getResourcesFromJar(path, dirUrl)
      case _                                            =>
        unsupportedOperationException(s"Cannot get resources for path $p")
    }
  }

  private def getResourcesFromJar(path: String, dirURL: jUrl): Array[String] = {
    val dirPath = dirURL.getPath
    val jarPath = dirPath.substring(5, dirPath indexOf "!")
    val jarFile = new JarFile(java.net.URLDecoder.decode(jarPath, "UTF-8"))
    andClose(jarFile) { jar =>
      def resources = jar.entries.iterator.map(_.getName).filter(_ startsWith path).map { name =>
        val entry = name substring path.length
        (entry indexOf "/") |> (i => if (i < 0) entry else entry.substring(0, i))
      }
      resources.toArray distinct HashEq.natural() toArray
    }
  }
}
