package psp
package std

import java.nio.{ file => jnf }
import java.nio.file.{ attribute => jnfa }

object Paths {
  def get(path: String): jnf.Path = jnf.Paths get path
}

object FileTime {
  val NoFileTime = this fromMillis MinLong

  def empty: FileTime                   = NoFileTime
  def fromMillis(value: Long): FileTime = jnfa.FileTime fromMillis value
}

object Files {
  def readAllBytes(path: jnf.Path): Array[Byte]                = jnf.Files.readAllBytes(path)
  def readAllLines(path: jnf.Path, cs: Charset): jList[String] = jnf.Files.readAllLines(path, cs)
  def readAllLines(path: jnf.Path): jList[String]              = readAllLines(path, defaultCharset)

  // def copy(x$1: java.io.InputStream,x$2: jnf.Path,x$3: Array[java.nio.file.CopyOption]): Long
  // def copy(x$1: jnf.Path,x$2: java.io.OutputStream): Long
  // def copy(x$1: jnf.Path,x$2: jnf.Path,x$3: Array[java.nio.file.CopyOption]): jnf.Path
  // def createDirectories(x$1: jnf.Path,x$2: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createDirectory(x$1: jnf.Path,x$2: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createFile(x$1: jnf.Path,x$2: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createLink(x$1: jnf.Path,x$2: jnf.Path): jnf.Path
  // def createSymbolicLink(x$1: jnf.Path,x$2: jnf.Path,x$3: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createTempDirectory(x$1: String,x$2: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createTempDirectory(x$1: jnf.Path,x$2: String,x$3: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createTempFile(x$1: String,x$2: String,x$3: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def createTempFile(x$1: jnf.Path,x$2: String,x$3: String,x$4: Array[java.nio.file.attribute.FileAttribute]): jnf.Path
  // def delete(x$1: jnf.Path): Unit
  // def deleteIfExists(x$1: jnf.Path): Boolean
  // def exists(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): Boolean
  // def getAttribute(x$1: jnf.Path,x$2: String,x$3: Array[java.nio.file.LinkOption]): Object
  // def getFileAttributeView(x$1: jnf.Path,x$2: Class,x$3: Array[java.nio.file.LinkOption]): java.nio.file.attribute.FileAttributeView
  // def getFileStore(x$1: jnf.Path): java.nio.file.FileStore
  // def getLastModifiedTime(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): java.nio.file.attribute.FileTime
  // def getOwner(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): java.nio.file.attribute.UserPrincipal
  // def getPosixFilePermissions(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): java.util.Set
  // def isDirectory(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): Boolean
  // def isExecutable(x$1: jnf.Path): Boolean
  // def isHidden(x$1: jnf.Path): Boolean
  // def isReadable(x$1: jnf.Path): Boolean
  // def isRegularFile(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): Boolean
  // def isSameFile(x$1: jnf.Path,x$2: jnf.Path): Boolean
  // def isSymbolicLink(x$1: jnf.Path): Boolean
  // def isWritable(x$1: jnf.Path): Boolean
  // def move(x$1: jnf.Path,x$2: jnf.Path,x$3: Array[java.nio.file.CopyOption]): jnf.Path
  // def newBufferedReader(x$1: jnf.Path,x$2: java.nio.charset.Charset): java.io.BufferedReader
  // def newBufferedWriter(x$1: jnf.Path,x$2: java.nio.charset.Charset,x$3: Array[java.nio.file.OpenOption]): java.io.BufferedWriter
  // def newByteChannel(x$1: jnf.Path,x$2: Array[java.nio.file.OpenOption]): java.nio.channels.SeekableByteChannel
  // def newByteChannel(x$1: jnf.Path,x$2: java.util.Set,x$3: Array[java.nio.file.attribute.FileAttribute]): java.nio.channels.SeekableByteChannel
  // def newDirectoryStream(x$1: jnf.Path): java.nio.file.DirectoryStream
  // def newDirectoryStream(x$1: jnf.Path,x$2: String): java.nio.file.DirectoryStream
  // def newDirectoryStream(x$1: jnf.Path,x$2: java.nio.file.DirectoryStream$Filter): java.nio.file.DirectoryStream
  // def newInputStream(x$1: jnf.Path,x$2: Array[java.nio.file.OpenOption]): java.io.InputStream
  // def newOutputStream(x$1: jnf.Path,x$2: Array[java.nio.file.OpenOption]): java.io.OutputStream
  // def notExists(x$1: jnf.Path,x$2: Array[java.nio.file.LinkOption]): Boolean
  // def probeContentType(x$1: jnf.Path): String
  // def readAllBytes(x$1: jnf.Path): Array[Byte]
  // def readAllLines(x$1: jnf.Path,x$2: java.nio.charset.Charset): java.util.List
  // def readAttributes(x$1: jnf.Path,x$2: Class,x$3: Array[java.nio.file.LinkOption]): java.nio.file.attribute.BasicFileAttributes
  // def readAttributes(x$1: jnf.Path,x$2: String,x$3: Array[java.nio.file.LinkOption]): java.util.Map
  // def readSymbolicLink(x$1: jnf.Path): jnf.Path
  // def setAttribute(x$1: jnf.Path,x$2: String,x$3: Object,x$4: Array[java.nio.file.LinkOption]): jnf.Path
  // def setLastModifiedTime(x$1: jnf.Path,x$2: java.nio.file.attribute.FileTime): jnf.Path
  // def setOwner(x$1: jnf.Path,x$2: java.nio.file.attribute.UserPrincipal): jnf.Path
  // def setPosixFilePermissions(x$1: jnf.Path,x$2: java.util.Set): jnf.Path
  // def size(x$1: jnf.Path): Long
  // def walkFileTree(x$1: jnf.Path,x$2: java.nio.file.FileVisitor): jnf.Path
  // def walkFileTree(x$1: jnf.Path,x$2: java.util.Set,x$3: Int,x$4: java.nio.file.FileVisitor): jnf.Path
  // def write(x$1: jnf.Path,x$2: Array[Byte],x$3: Array[java.nio.file.OpenOption]): jnf.Path
  // def write(x$1: jnf.Path,x$2: Iterable,x$3: java.nio.charset.Charset,x$4: Array[java.nio.file.OpenOption]): jnf.Path
}


object Arrays {
  // Static methods in java.util.Arrays
  //
  // boolean deepEquals(java.lang.Object[],java.lang.Object[])
  // boolean equals(boolean[],boolean[])
  // boolean equals(byte[],byte[])
  // boolean equals(char[],char[])
  // boolean equals(double[],double[])
  // boolean equals(float[],float[])
  // boolean equals(int[],int[])
  // boolean equals(java.lang.Object[],java.lang.Object[])
  // boolean equals(long[],long[])
  // boolean equals(short[],short[])
  // boolean[] copyOf(boolean[],int)
  // boolean[] copyOfRange(boolean[],int,int)
  // byte[] copyOf(byte[],int)
  // byte[] copyOfRange(byte[],int,int)
  // char[] copyOf(char[],int)
  // char[] copyOfRange(char[],int,int)
  // double[] copyOf(double[],int)
  // double[] copyOfRange(double[],int,int)
  // float[] copyOf(float[],int)
  // float[] copyOfRange(float[],int,int)
  // int binarySearch(byte[],byte)
  // int binarySearch(byte[],int,int,byte)
  // int binarySearch(char[],char)
  // int binarySearch(char[],int,int,char)
  // int binarySearch(double[],double)
  // int binarySearch(double[],int,int,double)
  // int binarySearch(float[],float)
  // int binarySearch(float[],int,int,float)
  // int binarySearch(int[],int)
  // int binarySearch(int[],int,int,int)
  // int binarySearch(java.lang.Object[],int,int,java.lang.Object)
  // int binarySearch(java.lang.Object[],int,int,java.lang.Object,java.util.Comparator)
  // int binarySearch(java.lang.Object[],java.lang.Object)
  // int binarySearch(java.lang.Object[],java.lang.Object,java.util.Comparator)
  // int binarySearch(long[],int,int,long)
  // int binarySearch(long[],long)
  // int binarySearch(short[],int,int,short)
  // int binarySearch(short[],short)
  // int deepHashCode(java.lang.Object[])
  // int hashCode(boolean[])
  // int hashCode(byte[])
  // int hashCode(char[])
  // int hashCode(double[])
  // int hashCode(float[])
  // int hashCode(int[])
  // int hashCode(java.lang.Object[])
  // int hashCode(long[])
  // int hashCode(short[])
  // int[] copyOf(int[],int)
  // int[] copyOfRange(int[],int,int)
  // java.lang.Object[] copyOf(java.lang.Object[],int)
  // java.lang.Object[] copyOf(java.lang.Object[],int,java.lang.Class)
  // java.lang.Object[] copyOfRange(java.lang.Object[],int,int)
  // java.lang.Object[] copyOfRange(java.lang.Object[],int,int,java.lang.Class)
  // java.lang.String deepToString(java.lang.Object[])
  // java.lang.String toString(boolean[])
  // java.lang.String toString(byte[])
  // java.lang.String toString(char[])
  // java.lang.String toString(double[])
  // java.lang.String toString(float[])
  // java.lang.String toString(int[])
  // java.lang.String toString(java.lang.Object[])
  // java.lang.String toString(long[])
  // java.lang.String toString(short[])
  // java.util.List asList(java.lang.Object[])
  // long[] copyOf(long[],int)
  // long[] copyOfRange(long[],int,int)
  // short[] copyOf(short[],int)
  // short[] copyOfRange(short[],int,int)
  // void fill(boolean[],boolean)
  // void fill(boolean[],int,int,boolean)
  // void fill(byte[],byte)
  // void fill(byte[],int,int,byte)
  // void fill(char[],char)
  // void fill(char[],int,int,char)
  // void fill(double[],double)
  // void fill(double[],int,int,double)
  // void fill(float[],float)
  // void fill(float[],int,int,float)
  // void fill(int[],int)
  // void fill(int[],int,int,int)
  // void fill(java.lang.Object[],int,int,java.lang.Object)
  // void fill(java.lang.Object[],java.lang.Object)
  // void fill(long[],int,int,long)
  // void fill(long[],long)
  // void fill(short[],int,int,short)
  // void fill(short[],short)
  // void sort(byte[])
  // void sort(byte[],int,int)
  // void sort(char[])
  // void sort(char[],int,int)
  // void sort(double[])
  // void sort(double[],int,int)
  // void sort(float[])
  // void sort(float[],int,int)
  // void sort(int[])
  // void sort(int[],int,int)
  // void sort(java.lang.Object[])
  // void sort(java.lang.Object[],int,int)
  // void sort(java.lang.Object[],int,int,java.util.Comparator)
  // void sort(java.lang.Object[],java.util.Comparator)
  // void sort(long[])
  // void sort(long[],int,int)
  // void sort(short[])
  // void sort(short[],int,int)
}
