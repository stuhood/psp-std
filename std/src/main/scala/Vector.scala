package psp
package std
package svec

import api.{ Size, Precise, Direct, Index, ForceShowDirect }
import scala.compat.Platform.arraycopy
import StdShow._

/** Based on the scala library Vector by Tiark Rompf.
 *    https://issues.scala-lang.org/browse/SI-4442
 *    https://issues.scala-lang.org/browse/SI-4495
 *    https://github.com/scala/scala/pull/3498
 */

object Vector {
  def empty[A] = NIL

  def newBuilder[@spec A]()                        = new VectorBuilder[A]()
  def apply[@spec A](xs: A*): Vector[A]            = newBuilder[A]() doto (b => xs foreach (b += _)) result
  def unapplySeq[A](x: Vector[A]): Some[Vector[A]] = Some(x)

  private[svec] val NIL = new Vector[Nothing](0, 0, 0)
  // Constants governing concat strategy for performance
  private[svec] final val Log2ConcatFaster = 5
  private[svec] final val TinyAppendFaster = 2
}

final class Vector[@spec +A](val startIndex: Int, val endIndex: Int, focus: Int) extends VectorPointer[A @uV] with Direct[A] with ForceShowDirect {
  self =>

  private[svec] var dirty = false

  def to_s = "[ " + (this map (_.any_s) mk_s ", ") + " ]"

  def length = endIndex - startIndex

  def foreach(f: A => Unit): Unit = iterator foreach f
  def size: Precise               = Precise(length)
  def elemAt(i: Index): A         = apply(i.getInt)
  def isEmpty                     = length == 0

  def :+[B >: A](elem: B): Vector[B]         = appendBack(elem)
  def +:[B >: A](elem: B): Vector[B]         = appendFront(elem)
  def ++[B >: A](that: Vector[B]): Vector[B] = that.foldl(this: Vector[B])(_ :+ _)

  private[svec] final def initIterator[B >: A](s: VectorIterator[B]): VectorIterator[B] = {
    s.initFrom(this)
    if (dirty) s.stabilize(focus)
    if (s.depth > 1) s.gotoPos(startIndex, startIndex ^ focus)
    s
  }

  def iterator: VectorIterator[A] =
    initIterator(new VectorIterator[A](startIndex, endIndex))

  // can still be improved
  def reverseIterator: scIterator[A] = new scIterator[A] {
    private[svec] var i = self.length
    def hasNext: Boolean = 0 < i
    def next(): A =
      if (0 < i) {
        i -= 1
        self(i)
      } else scala.collection.Iterator.empty.next()
  }

  // TODO: reverse

  // TODO: check performance of foreach/map etc. should override or not?
  // Ideally, clients will inline calls to map all the way down, including the iterator/builder methods.
  // In principle, escape analysis could even remove the iterator/builder allocations and do it
  // with local variables exclusively. But we're not quite there yet ...

  def apply(index: Int): A = {
    val idx = checkRangeConvert(index)
    //println("get elem: "+index + "/"+idx + "(focus:" +focus+" xor:"+(idx^focus)+" depth:"+depth+")")
    getElem(idx, idx ^ focus)
  }

  private[svec] def checkRangeConvert(index: Int) = {
    val idx = index + startIndex
    if (0 <= index && idx < endIndex)
      idx
    else
      throw new java.lang.IndexOutOfBoundsException(index.toString)
  }

  // semi-private[svec] api

  private[svec] def updateAt[B >: A](index: Int, elem: B): Vector[B] = {
    val idx = checkRangeConvert(index)
    val s = new Vector[B](startIndex, endIndex, idx)
    s.initFrom(this)
    s.dirty = dirty
    s.gotoPosWritable(focus, idx, focus ^ idx)  // if dirty commit changes; go to new pos and prepare for writing
    s.display0(idx & 0x1f) = elem.asInstanceOf[AnyRef]
    s
  }


  private[svec] def gotoPosWritable(oldIndex: Int, newIndex: Int, xor: Int) = if (dirty) {
    gotoPosWritable1(oldIndex, newIndex, xor)
  } else {
    gotoPosWritable0(newIndex, xor)
    dirty = true
  }

  private[svec] def gotoFreshPosWritable(oldIndex: Int, newIndex: Int, xor: Int) = {
    val wasDirty = dirty
    if (wasDirty)
      stabilize(oldIndex)

    gotoFreshPosWritable0(oldIndex, newIndex, xor)

    if (!wasDirty)
      dirty = true
  }

  private[svec] def appendFront[B >: A](value: B): Vector[B] = {
    if (endIndex != startIndex) {
      val blockIndex = (startIndex - 1) & ~31
      val lo = (startIndex - 1) & 31

      if (startIndex != blockIndex + 32) {
        val s = new Vector(startIndex - 1, endIndex, blockIndex)
        s.initFrom(this)
        s.dirty = dirty
        s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        s
      } else {

        val freeSpace = ((1<<5*(depth)) - endIndex) // free space at the right given the current tree-structure depth
        val shift = freeSpace & ~((1<<5*(depth-1))-1) // number of elements by which we'll shift right (only move at top level)
        val shiftBlocks = freeSpace >>> 5*(depth-1) // number of top-level blocks

        //println("----- appendFront " + value + " at " + (startIndex - 1) + " reached block start")
        if (shift != 0) {
          // case A: we can shift right on the top level

          if (depth > 1) {
            val newBlockIndex = blockIndex + shift
            val newFocus = focus + shift
            val s = new Vector(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.shiftTopLevel(0, shiftBlocks) // shift right by n blocks
            s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // maybe create pos; prepare for writing
            s.display0(lo) = value.asInstanceOf[AnyRef]
            //assert(depth == s.depth)
            s
          } else {
            val newBlockIndex = blockIndex + 32
            val newFocus = focus

            //assert(newBlockIndex == 0)
            //assert(newFocus == 0)

            val s = new Vector(startIndex - 1 + shift, endIndex + shift, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.shiftTopLevel(0, shiftBlocks) // shift right by n elements
            s.gotoPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // prepare for writing
            s.display0(shift-1) = value.asInstanceOf[AnyRef]
            s
          }
        } else if (blockIndex < 0) {
          // case B: we need to move the whole structure
          val move = (1 << 5*(depth+1)) - (1 << 5*(depth))

          val newBlockIndex = blockIndex + move
          val newFocus = focus + move


          val s = new Vector(startIndex - 1 + move, endIndex + move, newBlockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // could optimize: we know it will create a whole branch
          s.display0(lo) = value.asInstanceOf[AnyRef]
          //assert(s.depth == depth+1)
          s
        } else {
          val newBlockIndex = blockIndex
          val newFocus = focus

          val s = new Vector(startIndex - 1, endIndex, newBlockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
          s.display0(lo) = value.asInstanceOf[AnyRef]
          //assert(s.depth == depth)
          s
        }

      }
    } else {
      // empty vector, just insert single element at the back
      val elems = new Array[AnyRef](32)
      elems(31) = value.asInstanceOf[AnyRef]
      val s = new Vector(31,32,0)
      s.depth = 1
      s.display0 = elems
      s
    }
  }

  private[svec] def appendBack[B>:A](value: B): Vector[B] = {
    if (endIndex != startIndex) {
      val blockIndex = endIndex & ~31
      val lo = endIndex & 31

      if (endIndex != blockIndex) {
        val s = new Vector(startIndex, endIndex + 1, blockIndex)
        s.initFrom(this)
        s.dirty = dirty
        s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        s
      }
      else {
        val shift = startIndex & ~((1<<5*(depth-1))-1)
        val shiftBlocks = startIndex >>> 5*(depth-1)

        if (shift != 0) {
          if (depth > 1) {
            val newBlockIndex = blockIndex - shift
            val newFocus = focus - shift
            val s = new Vector(startIndex - shift, endIndex + 1 - shift, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.shiftTopLevel(shiftBlocks, 0) // shift left by n blocks
            s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(lo) = value.asInstanceOf[AnyRef]
            //assert(depth == s.depth)
            s
          } else {
            val newBlockIndex = blockIndex - 32
            val newFocus = focus

            //assert(newBlockIndex == 0)
            //assert(newFocus == 0)

            val s = new Vector(startIndex - shift, endIndex + 1 - shift, newBlockIndex)
            s.initFrom(this)
            s.dirty = dirty
            s.shiftTopLevel(shiftBlocks, 0) // shift right by n elements
            s.gotoPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
            s.display0(32 - shift) = value.asInstanceOf[AnyRef]
            s
          }
        } else {
          val newBlockIndex = blockIndex
          val newFocus = focus

          val s = new Vector(startIndex, endIndex + 1, newBlockIndex)
          s.initFrom(this)
          s.dirty = dirty
          s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
          s.display0(lo) = value.asInstanceOf[AnyRef]
          //assert(s.depth == depth+1) might or might not create new level!
          s
        }
      }
    } else {
      val elems = new Array[AnyRef](32)
      elems(0) = value.asInstanceOf[AnyRef]
      val s = new Vector(0,1,0)
      s.depth = 1
      s.display0 = elems
      s
    }
  }


  // low-level implementation (needs cleanup, maybe move to util class)

  private[svec] def shiftTopLevel(oldLeft: Int, newLeft: Int) = (depth - 1) match {
    case 0 => display0 = copyRange(display0, oldLeft, newLeft)
    case 1 => display1 = copyRange(display1, oldLeft, newLeft)
    case 2 => display2 = copyRange(display2, oldLeft, newLeft)
    case 3 => display3 = copyRange(display3, oldLeft, newLeft)
    case 4 => display4 = copyRange(display4, oldLeft, newLeft)
    case 5 => display5 = copyRange(display5, oldLeft, newLeft)
  }

  private[svec] def zeroLeft(array: Array[AnyRef], index: Int): Unit = {
    var i = 0; while (i < index) { array(i) = null; i+=1 }
  }

  private[svec] def zeroRight(array: Array[AnyRef], index: Int): Unit = {
    var i = index; while (i < array.length) { array(i) = null; i+=1 }
  }

  private[svec] def copyLeft(array: Array[AnyRef], right: Int): Array[AnyRef] =
    new Array[AnyRef](array.length) doto (a2 => arraycopy(array, 0, a2, 0, right))

  private[svec] def copyRight(array: Array[AnyRef], left: Int): Array[AnyRef] =
    new Array[AnyRef](array.length) doto (a2 => arraycopy(array, left, a2, left, a2.length - left))

  private[svec] def preClean(depth: Int) = {
    this.depth = depth
    (depth - 1) match {
      case 0 =>
        display1 = null
        display2 = null
        display3 = null
        display4 = null
        display5 = null
      case 1 =>
        display2 = null
        display3 = null
        display4 = null
        display5 = null
      case 2 =>
        display3 = null
        display4 = null
        display5 = null
      case 3 =>
        display4 = null
        display5 = null
      case 4 =>
        display5 = null
      case 5 =>
    }
  }

  // requires structure is at index cutIndex and writable at level 0
  private[svec] def cleanLeftEdge(cutIndex: Int) = {
    if (cutIndex < (1 << 5)) {
      zeroLeft(display0, cutIndex)
    } else
    if (cutIndex < (1 << 10)) {
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5))
    } else
    if (cutIndex < (1 << 15)) {
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10))
    } else
    if (cutIndex < (1 << 20)) {
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10) & 0x1f)
      display3 = copyRight(display3, (cutIndex >>> 15))
    } else
    if (cutIndex < (1 << 25)) {
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10) & 0x1f)
      display3 = copyRight(display3, (cutIndex >>> 15) & 0x1f)
      display4 = copyRight(display4, (cutIndex >>> 20))
    } else
    if (cutIndex < (1 << 30)) {
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10) & 0x1f)
      display3 = copyRight(display3, (cutIndex >>> 15) & 0x1f)
      display4 = copyRight(display4, (cutIndex >>> 20) & 0x1f)
      display5 = copyRight(display5, (cutIndex >>> 25))
    } else {
      throw new IllegalArgumentException()
    }
  }

  // requires structure is writable and at index cutIndex
  private[svec] def cleanRightEdge(cutIndex: Int) = {
    // we're actually sitting one block left if cutIndex lies on a block boundary
    // this means that we'll end up erasing the whole block!!

    if (cutIndex <= (1 << 5)) {
      zeroRight(display0, cutIndex)
    } else
    if (cutIndex <= (1 << 10)) {
      zeroRight(display0, ((cutIndex-1) & 0x1f) + 1)
      display1 = copyLeft(display1, (cutIndex >>>  5))
    } else
    if (cutIndex <= (1 << 15)) {
      zeroRight(display0, ((cutIndex-1) & 0x1f) + 1)
      display1 = copyLeft(display1, (((cutIndex-1) >>>  5) & 0x1f) + 1)
      display2 = copyLeft(display2, (cutIndex >>> 10))
    } else
    if (cutIndex <= (1 << 20)) {
      zeroRight(display0, ((cutIndex-1) & 0x1f) + 1)
      display1 = copyLeft(display1, (((cutIndex-1) >>>  5) & 0x1f) + 1)
      display2 = copyLeft(display2, (((cutIndex-1) >>> 10) & 0x1f) + 1)
      display3 = copyLeft(display3, (cutIndex >>> 15))
    } else
    if (cutIndex <= (1 << 25)) {
      zeroRight(display0, ((cutIndex-1) & 0x1f) + 1)
      display1 = copyLeft(display1, (((cutIndex-1) >>>  5) & 0x1f) + 1)
      display2 = copyLeft(display2, (((cutIndex-1) >>> 10) & 0x1f) + 1)
      display3 = copyLeft(display3, (((cutIndex-1) >>> 15) & 0x1f) + 1)
      display4 = copyLeft(display4, (cutIndex >>> 20))
    } else
    if (cutIndex <= (1 << 30)) {
      zeroRight(display0, ((cutIndex-1) & 0x1f) + 1)
      display1 = copyLeft(display1, (((cutIndex-1) >>>  5) & 0x1f) + 1)
      display2 = copyLeft(display2, (((cutIndex-1) >>> 10) & 0x1f) + 1)
      display3 = copyLeft(display3, (((cutIndex-1) >>> 15) & 0x1f) + 1)
      display4 = copyLeft(display4, (((cutIndex-1) >>> 20) & 0x1f) + 1)
      display5 = copyLeft(display5, (cutIndex >>> 25))
    } else {
      throw new IllegalArgumentException()
    }
  }

  private[svec] def requiredDepth(xor: Int) = {
    if (xor < (1 <<  5)) 1
    else if (xor < (1 << 10)) 2
    else if (xor < (1 << 15)) 3
    else if (xor < (1 << 20)) 4
    else if (xor < (1 << 25)) 5
    else if (xor < (1 << 30)) 6
    else throw new IllegalArgumentException()
  }

  private[svec] def dropFront0(cutIndex: Int): Vector[A] = {
    val blockIndex = cutIndex & ~31
    val xor = cutIndex ^ (endIndex - 1)
    val d = requiredDepth(xor)
    val shift = (cutIndex & ~((1 << (5*d))-1))

    // need to init with full display iff going to cutIndex requires swapping block at level >= d
    val s = new Vector(cutIndex-shift, endIndex-shift, blockIndex-shift)
    s.initFrom(this)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
    s.preClean(d)
    s.cleanLeftEdge(cutIndex - shift)
    s
  }

  private[svec] def dropBack0(cutIndex: Int): Vector[A] = {
    val blockIndex = (cutIndex - 1) & ~31
    val xor = startIndex ^ (cutIndex - 1)
    val d = requiredDepth(xor)
    val shift = (startIndex & ~((1 << (5*d))-1))
    val s = new Vector(startIndex-shift, cutIndex-shift, blockIndex-shift)

    s.initFrom(this)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex, focus ^ blockIndex)
    s.preClean(d)
    s.cleanRightEdge(cutIndex-shift)
    s
  }

}


class VectorIterator[@spec +A](_startIndex: Int, endIndex: Int) extends scIterator[A] with VectorPointer[A @uV] {
  private[svec] var blockIndex: Int = _startIndex & ~31
  private[svec] var lo: Int         = _startIndex & 31
  private[svec] var endLo           = math.min(endIndex - blockIndex, 32)
  private[svec] var _hasNext        = blockIndex + lo < endIndex

  def hasNext = _hasNext
  def next(): A = {
    if (!_hasNext) throw new NoSuchElementException("reached iterator end")

    val res = display0(lo).asInstanceOf[A]
    lo += 1

    if (lo == endLo) {
      if (blockIndex + lo < endIndex) {
        val newBlockIndex = blockIndex+32
        gotoNextBlockStart(newBlockIndex, blockIndex ^ newBlockIndex)

        blockIndex = newBlockIndex
        endLo = math.min(endIndex - blockIndex, 32)
        lo = 0
      } else {
        _hasNext = false
      }
    }

    res
  }

  private[svec] def remainingElementCount: Int = (endIndex - (blockIndex + lo)) max 0

  /** Creates a new vector which consists of elements remaining in this iterator.
   *  Such a vector can then be split into several vectors using methods like `take` and `drop`.
   */
  private[svec] def remainingVector: Vector[A] =
    new Vector(blockIndex + lo, endIndex, blockIndex + lo) doto (_ initFrom this)
}

final class VectorBuilder[@spec A]() extends scmBuilder[A, Vec[A]] with VectorPointer[A @uV] {
  // possible alternative: start with display0 = null, blockIndex = -32, lo = 32
  // to avoid allocating initial array if the result will be empty anyways

  display0 = new Array[AnyRef](32)
  depth = 1

  private[svec] var blockIndex = 0
  private[svec] var lo = 0

  def += (elem: A): this.type = {
    if (lo >= display0.length) {
      val newBlockIndex = blockIndex+32
      gotoNextBlockStartWritable(newBlockIndex, blockIndex ^ newBlockIndex)
      blockIndex = newBlockIndex
      lo = 0
    }
    display0(lo) = elem.asInstanceOf[AnyRef]
    lo += 1
    this
  }

  def result: Vector[A] = {
    val size = blockIndex + lo
    if (size == 0)
      return Vector.empty

    val s = new Vector[A](0, size, 0) // should focus front or back?
    s.initFrom(this)
    if (depth > 1) s.gotoPos(0, size - 1) // we're currently focused to size - 1, not size!
    s
  }

  def clear(): Unit = {
    display0 = new Array[AnyRef](32)
    depth = 1
    blockIndex = 0
    lo = 0
  }
}

private[svec] trait VectorPointer[@spec T] {
    private[svec] var depth: Int              = _
    private[svec] var display0: Array[AnyRef] = _
    private[svec] var display1: Array[AnyRef] = _
    private[svec] var display2: Array[AnyRef] = _
    private[svec] var display3: Array[AnyRef] = _
    private[svec] var display4: Array[AnyRef] = _
    private[svec] var display5: Array[AnyRef] = _

    // used
    private[svec] final def initFrom[U](that: VectorPointer[U]): Unit = initFrom(that, that.depth)

    private[svec] final def initFrom[U](that: VectorPointer[U], depth: Int) = {
      this.depth = depth
      (depth - 1) match {
        case -1 =>
        case 0 =>
          display0 = that.display0
        case 1 =>
          display1 = that.display1
          display0 = that.display0
        case 2 =>
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
        case 3 =>
          display3 = that.display3
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
        case 4 =>
          display4 = that.display4
          display3 = that.display3
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
        case 5 =>
          display5 = that.display5
          display4 = that.display4
          display3 = that.display3
          display2 = that.display2
          display1 = that.display1
          display0 = that.display0
      }
    }


    // requires structure is at pos oldIndex = xor ^ index
    private[svec] final def getElem(index: Int, xor: Int): T = {
      if (xor < (1 << 5)) { // level = 0
        display0(index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 10)) { // level = 1
        display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 15)) { // level = 2
        display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 20)) { // level = 3
        display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 25)) { // level = 4
        display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else
      if (xor < (1 << 30)) { // level = 5
        display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]((index >> 20) & 31).asInstanceOf[Array[AnyRef]]((index >> 15) & 31).asInstanceOf[Array[AnyRef]]((index >> 10) & 31).asInstanceOf[Array[AnyRef]]((index >> 5) & 31).asInstanceOf[Array[AnyRef]](index & 31).asInstanceOf[T]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    // go to specific position
    // requires structure is at pos oldIndex = xor ^ index,
    // ensures structure is at pos index
    private[svec] final def gotoPos(index: Int, xor: Int): Unit = {
      if (xor < (1 << 5)) { // level = 0 (could maybe removed)
      } else
      if (xor < (1 << 10)) { // level = 1
        display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 15)) { // level = 2
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 20)) { // level = 3
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 25)) { // level = 4
        display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 30)) { // level = 5
        display4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1((index >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    // USED BY ITERATOR

    // xor: oldIndex ^ index
    private[svec] final def gotoNextBlockStart(index: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 10)) { // level = 1
        display0 = display1((index >> 5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 15)) { // level = 2
        display1 = display2((index >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 20)) { // level = 3
        display2 = display3((index >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 25)) { // level = 4
        display3 = display4((index >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 30)) { // level = 5
        display4 = display5((index >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = display4(0).asInstanceOf[Array[AnyRef]]
        display2 = display3(0).asInstanceOf[Array[AnyRef]]
        display1 = display2(0).asInstanceOf[Array[AnyRef]]
        display0 = display1(0).asInstanceOf[Array[AnyRef]]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    // USED BY BUILDER

    // xor: oldIndex ^ index
    private[svec] final def gotoNextBlockStartWritable(index: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 10)) { // level = 1
        if (depth == 1) { display1 = new Array(32); display1(0) = display0; depth+=1}
        display0 = new Array(32)
        display1((index >>  5) & 31) = display0
      } else
      if (xor < (1 << 15)) { // level = 2
        if (depth == 2) { display2 = new Array(32); display2(0) = display1; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
      } else
      if (xor < (1 << 20)) { // level = 3
        if (depth == 3) { display3 = new Array(32); display3(0) = display2; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
        display3((index >> 15) & 31) = display2
      } else
      if (xor < (1 << 25)) { // level = 4
        if (depth == 4) { display4 = new Array(32); display4(0) = display3; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
        display3((index >> 15) & 31) = display2
        display4((index >> 20) & 31) = display3
      } else
      if (xor < (1 << 30)) { // level = 5
        if (depth == 5) { display5 = new Array(32); display5(0) = display4; depth+=1}
        display0 = new Array(32)
        display1 = new Array(32)
        display2 = new Array(32)
        display3 = new Array(32)
        display4 = new Array(32)
        display1((index >>  5) & 31) = display0
        display2((index >> 10) & 31) = display1
        display3((index >> 15) & 31) = display2
        display4((index >> 20) & 31) = display3
        display5((index >> 25) & 31) = display4
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }

    // STUFF BELOW USED BY APPEND / UPDATE
    private[svec] final def copyOf(a: Array[AnyRef]) =
      new Array[AnyRef](a.length) doto (b => arraycopy(a, 0, b, 0, a.length))

    private[svec] final def nullSlotAndCopy(array: Array[AnyRef], index: Int) = {
      val x = array(index)
      array(index) = null
      copyOf(x.asInstanceOf[Array[AnyRef]])
    }

    // make sure there is no aliasing
    // requires structure is at pos index
    // ensures structure is clean and at pos index and writable at all levels except 0

    private[svec] final def stabilize(index: Int) = (depth - 1) match {
      case 5 =>
        display5 = copyOf(display5)
        display4 = copyOf(display4)
        display3 = copyOf(display3)
        display2 = copyOf(display2)
        display1 = copyOf(display1)
        display5((index >> 25) & 31) = display4
        display4((index >> 20) & 31) = display3
        display3((index >> 15) & 31) = display2
        display2((index >> 10) & 31) = display1
        display1((index >>  5) & 31) = display0
      case 4 =>
        display4 = copyOf(display4)
        display3 = copyOf(display3)
        display2 = copyOf(display2)
        display1 = copyOf(display1)
        display4((index >> 20) & 31) = display3
        display3((index >> 15) & 31) = display2
        display2((index >> 10) & 31) = display1
        display1((index >>  5) & 31) = display0
      case 3 =>
        display3 = copyOf(display3)
        display2 = copyOf(display2)
        display1 = copyOf(display1)
        display3((index >> 15) & 31) = display2
        display2((index >> 10) & 31) = display1
        display1((index >>  5) & 31) = display0
      case 2 =>
        display2 = copyOf(display2)
        display1 = copyOf(display1)
        display2((index >> 10) & 31) = display1
        display1((index >>  5) & 31) = display0
      case 1 =>
        display1 = copyOf(display1)
        display1((index >>  5) & 31) = display0
      case 0 =>
    }

    /// USED IN UPDATE AND APPEND BACK

    // prepare for writing at an existing position

    // requires structure is clean and at pos oldIndex = xor ^ newIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[svec] final def gotoPosWritable0(newIndex: Int, xor: Int): Unit = (depth - 1) match {
      case 5 =>
        display5 = copyOf(display5)
        display4 = nullSlotAndCopy(display5, (newIndex >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = nullSlotAndCopy(display4, (newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      case 4 =>
        display4 = copyOf(display4)
        display3 = nullSlotAndCopy(display4, (newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      case 3 =>
        display3 = copyOf(display3)
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      case 2 =>
        display2 = copyOf(display2)
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      case 1 =>
        display1 = copyOf(display1)
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      case 0 =>
        display0 = copyOf(display0)
    }


    // requires structure is dirty and at pos oldIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[svec] final def gotoPosWritable1(oldIndex: Int, newIndex: Int, xor: Int): Unit = {
      if (xor < (1 <<  5)) { // level = 0
        display0 = copyOf(display0)
      } else
      if (xor < (1 << 10)) { // level = 1
        display1 = copyOf(display1)
        display1((oldIndex >> 5) & 31) = display0
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31)
      } else
      if (xor < (1 << 15)) { // level = 2
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 20)) { // level = 3
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 25)) { // level = 4
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display4 = copyOf(display4)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display4((oldIndex >> 20) & 31) = display3
        display3 = nullSlotAndCopy(display4, (newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else
      if (xor < (1 << 30)) { // level = 5
        display1 = copyOf(display1)
        display2 = copyOf(display2)
        display3 = copyOf(display3)
        display4 = copyOf(display4)
        display5 = copyOf(display5)
        display1((oldIndex >>  5) & 31) = display0
        display2((oldIndex >> 10) & 31) = display1
        display3((oldIndex >> 15) & 31) = display2
        display4((oldIndex >> 20) & 31) = display3
        display5((oldIndex >> 25) & 31) = display4
        display4 = nullSlotAndCopy(display5, (newIndex >> 25) & 31).asInstanceOf[Array[AnyRef]]
        display3 = nullSlotAndCopy(display4, (newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        display2 = nullSlotAndCopy(display3, (newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        display1 = nullSlotAndCopy(display2, (newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        display0 = nullSlotAndCopy(display1, (newIndex >>  5) & 31).asInstanceOf[Array[AnyRef]]
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }


    // USED IN DROP

    private[svec] final def copyRange(array: Array[AnyRef], oldLeft: Int, newLeft: Int) = {
      val elems = new Array[AnyRef](32)
      arraycopy(array, oldLeft, elems, newLeft, 32 - math.max(newLeft,oldLeft))
      elems
    }

    // USED IN APPEND
    // create a new block at the bottom level (and possibly nodes on its path) and prepares for writing

    // requires structure is clean and at pos oldIndex,
    // ensures structure is dirty and at pos newIndex and writable at level 0
    private[svec] final def gotoFreshPosWritable0(oldIndex: Int, newIndex: Int, xor: Int): Unit = { // goto block start pos
      if (xor < (1 << 5)) { // level = 0
        //println("XXX clean with low xor")
      } else
      if (xor < (1 << 10)) { // level = 1
        if (depth == 1) {
          display1 = new Array(32)
          display1((oldIndex >>  5) & 31) = display0
          depth +=1
        }
        display0 = new Array(32)
      } else
      if (xor < (1 << 15)) { // level = 2
        if (depth == 2) {
          display2 = new Array(32)
          display2((oldIndex >> 10) & 31) = display1
          depth +=1
        }
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else
      if (xor < (1 << 20)) { // level = 3
        if (depth == 3) {
          display3 = new Array(32)
          display3((oldIndex >> 15) & 31) = display2
          depth +=1
        }
        display2 = display3((newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else
      if (xor < (1 << 25)) { // level = 4
        if (depth == 4) {
          display4 = new Array(32)
          display4((oldIndex >> 20) & 31) = display3
          depth +=1
        }
        display3 = display4((newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        if (display3 == null) display3 = new Array(32)
        display2 = display3((newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else
      if (xor < (1 << 30)) { // level = 5
        if (depth == 5) {
          display5 = new Array(32)
          display5((oldIndex >>  25) & 31) = display4
          depth +=1
        }
        display4 = display5((newIndex >> 25) & 31).asInstanceOf[Array[AnyRef]]
        if (display4 == null) display4 = new Array(32)
        display3 = display4((newIndex >> 20) & 31).asInstanceOf[Array[AnyRef]]
        if (display3 == null) display3 = new Array(32)
        display2 = display3((newIndex >> 15) & 31).asInstanceOf[Array[AnyRef]]
        if (display2 == null) display2 = new Array(32)
        display1 = display2((newIndex >> 10) & 31).asInstanceOf[Array[AnyRef]]
        if (display1 == null) display1 = new Array(32)
        display0 = new Array(32)
      } else { // level = 6
        throw new IllegalArgumentException()
      }
    }
}
