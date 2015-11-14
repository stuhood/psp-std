package psp
package std

import api._, StdEq._

/** Wrapper for java.lang.Class/ClassLoader and other java friends.
 */
object NullClassLoader extends jClassLoader
object NullInputStream extends InputStream { def read(): Int = -1 }

final class JavaClassImpl(val clazz: jClass) extends AnyVal with ForceShowDirect with JavaClass
final class JavaClassLoaderImpl(val loader: jClassLoader) extends AnyVal with ForceShowDirect with JavaClassLoader
final case class JavaMapEntry[K, V](pair: K -> V) extends jMapEntry[K, V] with (K -> V) {
  def _1                = fst(pair)
  def _2                = snd(pair)
  def getKey            = _1
  def getValue          = _2
  def setValue(x: V): V = unsupportedOperationException("setValue")
}

class JavaEnumeration[A](enum: jEnumeration[A]) {
  def iterator: BiIterator[A] = BiIterator enumeration enum
}

trait JavaClass extends Any {
  def clazz: jClass

  private def toPolicy(x: jClass): JavaClass = new JavaClassImpl(x)
  def javaClass: JavaClass = this // implicit creation hook

  def isAnnotation     = clazz.isAnnotation
  def isAnonymousClass = clazz.isAnonymousClass
  def isArray          = clazz.isArray
  def isEnum           = clazz.isEnum
  def isInterface      = clazz.isInterface
  def isLocalClass     = clazz.isLocalClass
  def isMemberClass    = clazz.isMemberClass
  def isPrimitive      = clazz.isPrimitive
  def isSynthetic      = clazz.isSynthetic

  def ancestorNames: Direct[String]         = ancestors mapNow (_.rawName)
  def ancestors: Direct[JavaClass]          = this transitiveClosure (_.parents) toDirect
  def exists: Boolean                       = clazz != null
  def fields: Direct[jField]                = clazz.getFields.toDirect
  def getCanonicalName: String              = clazz.getCanonicalName
  def getClassLoader: ClassLoader           = clazz.getClassLoader
  def getClasses: Direct[JavaClass]         = clazz.getClasses.toDirect mapNow toPolicy
  def getComponentType: JavaClass           = clazz.getComponentType
  def getDeclaredClasses: Direct[JavaClass] = clazz.getDeclaredClasses.toDirect mapNow toPolicy
  def getDeclaringClass: JavaClass          = clazz.getDeclaringClass
  def getEnclosingClass: JavaClass          = clazz.getEnclosingClass
  def getInterfaces: Direct[JavaClass]      = clazz.getInterfaces.toDirect mapNow toPolicy
  def getSuperclass: Option[JavaClass]      = Option(clazz.getSuperclass) map toPolicy
  def hasModuleName: Boolean                = rawName endsWith "$"
  def methods: Direct[jMethod]              = clazz.getMethods.toDirect
  def nameSegments: Direct[String]          = rawName.dottedSegments
  def parentInterfaces: Direct[JavaClass]   = clazz.getInterfaces.toDirect mapNow toPolicy
  def parents: Direct[JavaClass]            = getSuperclass.toDirect ++ parentInterfaces
  def qualifiedName: String                 = rawName.mapSplit('.')(decodeName)
  def rawName: String                       = clazz.getName
  def shortName: String                     = unqualifiedName
  def to_s: String                          = s"$clazz"
  def unqualifiedName: String               = decodeName(nameSegments.last)
}

trait JavaClassLoader extends Any {
  def loader: jClassLoader

  def parentChain: Each[jClassLoader] = {
    def loop(cl: jClassLoader): View[jClassLoader] = cl match {
      case null => exView()
      case _    => cl +: loop(cl.getParent)
    }
    loop(loader)
  }
  def uris: Each[jUri] = loader match {
    case cl: URLClassLoader => cl.getURLs.toEach map (_.toURI)
    case _                  => emptyValue
  }
  def to_s: String = s"$loader"
}
