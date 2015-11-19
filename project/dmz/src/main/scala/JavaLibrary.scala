package psp
package dmz

import scala._
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import java.nio.{ file => jnf }
import jnf.{ attribute => jnfa }
import java.lang.String

object DmzAliases extends JavaLibrary with ScalaLibrary

trait JavaLibrary extends Any {
  // Exceptional factories.
  def assertionError(msg: String): Nothing                = throw new AssertionError(msg)
  def illegalArgumentException(msg: Any): Nothing         = throw new IllegalArgumentException(s"$msg")
  def noSuchElementException(msg: String): Nothing        = throw new NoSuchElementException(msg)
  def runtimeException(msg: String): Nothing              = throw new RuntimeException(msg)
  def unsupportedOperationException(msg: String): Nothing = throw new UnsupportedOperationException(msg)
  def indexOutOfBoundsException(msg: Any): Nothing        = throw new IndexOutOfBoundsException(s"$msg")

  // A selection of popular static methods from javaland.
  def currentThread: Thread           = java.lang.Thread.currentThread
  def defaultCharset: Charset         = java.nio.charset.Charset.defaultCharset
  def fileSeparator: String           = java.io.File.separator
  def lineSeparator: String           = java.lang.System.getProperty("line.separator")
  def milliTime: Long                 = java.lang.System.currentTimeMillis
  def nanoTime: Long                  = java.lang.System.nanoTime
  def systemClassLoader: ClassLoader  = java.lang.ClassLoader.getSystemClassLoader
  def contextClassLoader: ClassLoader = currentThread.getContextClassLoader
  def utf8Charset: Charset            = java.nio.charset.Charset forName "UTF-8"
  def threadSleep(ms: Long): Unit     = java.lang.Thread.sleep(ms)
  def threadYield(): Unit             = java.lang.Thread.`yield`

  // Exceptions and Throwables.
  type AssertionError                = java.lang.AssertionError
  type ClassCastException            = java.lang.ClassCastException
  type Exception                     = java.lang.Exception
  type IOException                   = java.io.IOException
  type IllegalArgumentException      = java.lang.IllegalArgumentException
  type NoSuchElementException        = java.util.NoSuchElementException
  type RuntimeException              = java.lang.RuntimeException
  type Throwable                     = java.lang.Throwable
  type UnsupportedOperationException = java.lang.UnsupportedOperationException

  // java types which I acknowledge as victors in the battle for simple names.
  type AtomicInteger            = java.util.concurrent.atomic.AtomicInteger
  type BufferedInputStream      = java.io.BufferedInputStream
  type ByteBuffer               = java.nio.ByteBuffer
  type CharSequence             = java.lang.CharSequence
  type Charset                  = java.nio.charset.Charset
  type ClassLoader              = java.lang.ClassLoader
  type Class[A]                 = java.lang.Class[A]
  type Closeable                = java.io.Closeable
  type Comparable[A]            = java.lang.Comparable[A]
  type Comparator[-A]           = java.util.Comparator[A @uV]
  type DirectoryStreamFilter[A] = jnf.DirectoryStream.Filter[A]
  type DirectoryStream[A]       = jnf.DirectoryStream[A]
  type FileTime                 = jnfa.FileTime
  type InputStream              = java.io.InputStream
  type OutputStream             = java.io.OutputStream
  type Path                     = jnf.Path
  type PrintStream              = java.io.PrintStream
  type Thread                   = java.lang.Thread

  // java types for which the battle rages on.
  type jArrayList[A]            = java.util.ArrayList[A]
  type jClass                   = java.lang.Class[_]
  type jClassLoader             = java.lang.ClassLoader
  type jCollection[A]           = java.util.Collection[A]
  type jConcurrentHashMap[K, V] = java.util.concurrent.ConcurrentHashMap[K, V]
  type jConcurrentMap[K, V]     = java.util.concurrent.ConcurrentMap[K, V]
  type jDate                    = java.util.Date
  type jEnum[E <: jEnum[E]]     = java.lang.Enum[E]
  type jEnumeration[A]          = java.util.Enumeration[A]
  type jField                   = java.lang.reflect.Field
  type jFile                    = java.io.File
  type jFuture[A]               = java.util.concurrent.Future[A]
  type jHashMap[K, V]           = java.util.HashMap[K, V]
  type jHashSet[A]              = java.util.HashSet[A]
  type jIterable[+A]            = java.lang.Iterable[A @uV]
  type jIterator[+A]            = java.util.Iterator[A @uV]
  type jList[A]                 = java.util.List[A]
  type jMapEntry[K, V]          = java.util.Map.Entry[K, V]
  type jMap[K, V]               = java.util.Map[K, V]
  type jMethod                  = java.lang.reflect.Method
  type jSet[A]                  = java.util.Set[A]
  type jSortedMap[K, V]         = java.util.SortedMap[K, V]
  type jSortedSet[A]            = java.util.SortedSet[A]
  type jTreeMap[K, V]           = java.util.TreeMap[K, V]
  type jTreeSet[A]              = java.util.TreeSet[A]
  type jUri                     = java.net.URI
  type jUrl                     = java.net.URL
}
