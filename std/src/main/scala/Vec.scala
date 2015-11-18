package psp
package std

import api._, StdShow._
import java.lang.System.arraycopy
import Api.SpecTypes
import Vec.levelOf

/** Based on the scala library Vector by Tiark Rompf.
 *    https://issues.scala-lang.org/browse/SI-4442
 *    https://issues.scala-lang.org/browse/SI-4495
 *    https://github.com/scala/scala/pull/3498
 */

object Vec {
  def empty[@spec(SpecTypes) A] : Vec[A]                        = NIL.castTo[Vec[A]]
  def apply[@spec(SpecTypes) A](xs: A*): Vec[A]                 = newBuilder[A] build (Direct fromScala xs)
  def unapplySeq[@spec(SpecTypes) A](x: Vec[A]): Some[scSeq[A]] = Some(x.seq)
  def newBuilder[@spec(SpecTypes) A](): Builder[A]              = new Builder[A]()

  private[std] val NIL = new Vec[Any](0, 0, 0)
  // Constants governing concat strategy for performance
  private[std] final val Log2ConcatFaster = 5
  private[std] final val TinyAppendFaster = 2

  def levelOf(index: Int): Int = (
    if (index < (1 << 5)) 0
    else if (index < (1 << 10)) 1
    else if (index < (1 << 15)) 2
    else if (index < (1 << 20)) 3
    else if (index < (1 << 25)) 4
    else if (index < (1 << 30)) 5
    else 6
  )

  final case class Split[@spec(SpecTypes) A](left: Vec[A], right: Vec[A]) extends api.SplitInvariantM[Vec, A] {
    def mapLeft(f: ToSelf[Vec[A]]): Split[A]  = Split(f(left), right)
    def mapRight(f: ToSelf[Vec[A]]): Split[A] = Split(left, f(right))
    def rejoin: Vec[A]                        = left ++ right
  }

  final class Builder[@spec(SpecTypes) A]() extends Builds[A, Vec[A]] with VectorPointer[A @uV] {
    // possible alternative: start with display0 = null, blockIndex = -32, lo = 32
    // to avoid allocating initial array if the result will be empty anyways
    this.display0 = new Array[AnyRef](32)
    this.depth    = 1

    private[this] var blockIndex = 0
    private[this] var lo = 0

    def build(xs: Each[A]): Vec[A] = {
      xs foreach add
      result
    }
    def add(elem: A): Unit = {
      if (lo >= display0.length) {
        val newBlockIndex = blockIndex+32
        gotoNextBlockStartWritable(newBlockIndex, blockIndex ^ newBlockIndex)
        blockIndex = newBlockIndex
        lo = 0
      }
      display0(lo) = elem.asInstanceOf[AnyRef]
      lo += 1
    }
    def result(): Vec[A] = {
      val size = blockIndex + lo
      if (size == 0)
        return Vec.empty

      val s = new Vec[A](0, size, 0) // should focus front or back?
      s initFrom this
      if (depth > 1) s.gotoPos(0, size - 1) // we're currently focused to size - 1, not size!
      s
    }
  }
}

final class Vec[@spec(SpecTypes) A](val startIndex: Int, val endIndex: Int, focus: Int) extends VectorPointer[A @uV] with Direct[A] with ForceShowDirect {
  self =>

  @inline private def preStartIndex = startIndex - 1

  private def nextVec(s: Int, e: Int, f: Int): Vec[A] = new Vec[A](s, e, f)
  private def newVec(s: Int, e: Int, f: Int): Vec[A]  = nextVec(s, e, f) doto (_ initFrom this)

  private def updateDirty(s: Vec[A]): Vec[A] = { s.dirty = this.dirty ; s }

  private def newVecInFront(shift: Int, newBlockIndex: Int): Vec[A] =
    updateDirty(newVec(preStartIndex + shift, endIndex + shift, newBlockIndex))

  private def newVecInBack(shift: Int, newBlockIndex: Int): Vec[A] =
    updateDirty(newVec(startIndex - shift, endIndex + 1 - shift, newBlockIndex))

  private[std] var dirty = false

  def to_s = "[ " + (this map (_.any_s) mk_s ", ") + " ]"

  def lastIntIndex        = length - 1
  def length              = endIndex - startIndex
  def size: Precise       = Precise(length)
  def elemAt(i: Index): A = apply(i.getInt)
  def isEmpty: Boolean    = length == 0
  def nonEmpty: Boolean   = length > 0

  @inline def foreach(f: A => Unit): Unit = {
    if (nonEmpty)
      lowlevel.ll.foreachConsecutive(0, lastIntIndex, i => f(apply(i)))
  }

  def take(n: Int): Vec[A] =
    if (n <= 0) Vec.empty
    else if (n >= length) this
    else dropBack0(startIndex + n)

  def drop(n: Int): Vec[A] =
    if (n <= 0) this
    else if (n >= length) Vec.empty
    else dropFront0(startIndex + n)

  def takeRight(n: Int): Vec[A] =
    if (n <= 0) Vec.empty
    else if (n >= length) this
    else dropFront0(endIndex - n)

  def dropRight(n: Int): Vec[A] =
    if (n <= 0) this
    else if (endIndex - n > startIndex) dropBack0(endIndex - n)
    else Vec.empty

  def head: A      = if (nonEmpty) apply(0) else unsupportedOperationException("empty.head")
  def tail: Vec[A] = if (nonEmpty) drop(1) else unsupportedOperationException("empty.tail")
  def last: A      = if (nonEmpty) apply(length - 1) else unsupportedOperationException("empty.last")
  def init: Vec[A] = if (nonEmpty) dropRight(1) else unsupportedOperationException("empty.init")

  def slice(start: Int, end: Int): Vec[A]  = this take end drop start
  def splitAt(n: Int):  Vec.Split[A]       = Vec.Split(this take n, this drop n)
  def span(p: ToBool[A]): Vec.Split[A]     = splitAt(iterator count p)
  def takeWhile(p: ToBool[A]): Vec[A]      = take(iterator count p)
  def dropWhile(p: ToBool[A]): Vec[A]      = drop(iterator count p)
  def takeRightWhile(p: ToBool[A]): Vec[A] = takeRight(reverseIterator count p)
  def dropRightWhile(p: ToBool[A]): Vec[A] = dropRight(reverseIterator count p)

  @inline def foldl[@spec(SpecTypes) B](zero: B)(f: (B, A) => B): B = {
    var res = zero
    if (length > 0)
      lowlevel.ll.foreachConsecutive(0, lastIntIndex, i => res = f(res, apply(i)))
    res
  }

  def updated(i: Index, elem: A): Vec[A] = updateAt(i.getInt, elem)
  def :+(elem: A): Vec[A] = appendBack(elem)
  def +:(elem: A): Vec[A] = appendFront(elem)

  def ++(that: Vec[A]): Vec[A] = (
    if (that.isEmpty) this
    else if (this.isEmpty) that
    else {
      val b = Vec.newBuilder[A]
      this foreach b.add
      that foreach b.add
      b.result
    }
  )

  private[std] final def initIterator(s: VectorIterator[A]): VectorIterator[A] = {
    s.initFrom(this)
    if (dirty) s.stabilize(focus)
    if (s.depth > 1) s.gotoPos(startIndex, startIndex ^ focus)
    s
  }

  def iterator: VectorIterator[A]                          = initIterator(new VectorIterator[A](startIndex, endIndex))
  def reverseIterator: BiIterator.ReverseDirectIterator[A] = BiIterator direct this reverseIterator
  def reverse: Vec[A]                                      = reverseIterator.toVec
  def apply(index: Int): A                                 = getElemWithFocus(checkRangeConvert(index))

  private def getElemWithFocus(idx: Int): A = getElem(idx, idx ^ focus)
  private[std] def checkRangeConvert(index: Int): Int = (
    if (0 <= index && index < length) index + startIndex
    else indexOutOfBoundsException(index)
  )

  // semi-private[std] api

  private[std] def gotoPosWritable(oldIndex: Int, newIndex: Int): Unit = {
    if (dirty)
      gotoPosWritable1(oldIndex, newIndex)
    else {
      gotoPosWritable0(newIndex)
      dirty = true
    }
  }

  private[std] def gotoFreshPosWritable(oldIndex: Int, newIndex: Int, xor: Int): Unit = {
    val wasDirty = dirty
    if (wasDirty)
      stabilize(oldIndex)

    gotoFreshPosWritable0(oldIndex, newIndex, xor)

    if (!wasDirty)
      dirty = true
  }

  private def updateAt(index: Int, elem: A): Vec[A] = {
    val idx = checkRangeConvert(index)
    val s = updateDirty(newVec(startIndex, endIndex, idx))
    s.gotoPosWritable(focus, idx)  // if dirty commit changes; go to new pos and prepare for writing
    s.display0(idx & 0x1f) = elem.asInstanceOf[AnyRef]
    s
  }

  private[std] def appendFront(value: A): Vec[A] = {
    if (endIndex == startIndex)
      return makeSingletonAtBack(value)

    val blockIndex = (preStartIndex) & ~31
    val lo         = (preStartIndex) & 31

    if (startIndex != blockIndex + 32) {
      val s = newVecInFront(0, blockIndex)
      s.gotoPosWritable(focus, blockIndex)
      s.display0(lo) = value.asInstanceOf[AnyRef]
      s
    }
    else {
      val freeSpace   = ((1 << 5*(depth)) - endIndex) // free space at the right given the current tree-structure depth
      val shift       = freeSpace & ~((1 << 5*(depth-1))-1) // number of elements by which we'll shift right (only move at top level)
      val shiftBlocks = freeSpace >>> 5*(depth-1) // number of top-level blocks

      //println("----- appendFront " + value + " at " + (preStartIndex) + " reached block start")
      if (shift != 0) {
        // case A: we can shift right on the top level

        if (depth > 1) {
          val newBlockIndex = blockIndex + shift
          val newFocus = focus + shift
          val s = newVecInFront(shift, newBlockIndex)
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

          val s = newVecInFront(shift, newBlockIndex)
          s.shiftTopLevel(0, shiftBlocks) // shift right by n elements
          s.gotoPosWritable(newFocus, newBlockIndex) // prepare for writing
          s.display0(shift-1) = value.asInstanceOf[AnyRef]
          s
        }
      } else if (blockIndex < 0) {
        // case B: we need to move the whole structure
        val move = (1 << 5*(depth+1)) - (1 << 5*(depth))

        val newBlockIndex = blockIndex + move
        val newFocus = focus + move


        val s = newVecInFront(move, newBlockIndex)
        s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex) // could optimize: we know it will create a whole branch
        s.display0(lo) = value.asInstanceOf[AnyRef]
        //assert(s.depth == depth+1)
        s
      } else {
        val newBlockIndex = blockIndex
        val newFocus = focus

        val s = newVecInFront(0, newBlockIndex)
        s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        //assert(s.depth == depth)
        s
      }
    }
  }

  private def newSingleArray(startIndex: Int, endIndex: Int, elem: A): Vec[A] = {
    val elems = allocateArray(32)
    elems(startIndex) = elem.asInstanceOf[AnyRef]
    val s = nextVec(startIndex, endIndex, 0)
    s.depth = 1
    s.display0 = elems
    s
  }

  private def makeSingletonAtBack(value: A): Vec[A]  = newSingleArray(31, 32, value)
  private def makeSingletonAtFront(value: A): Vec[A] = newSingleArray(0, 1, value)

  private[std] def appendBack(value: A): Vec[A] = {
    if (endIndex == startIndex)
      return makeSingletonAtFront(value)

    val blockIndex = endIndex & ~31
    val lo         = endIndex & 31

    if (endIndex != blockIndex) {
      val s = newVecInBack(0, blockIndex)
      s.gotoPosWritable(focus, blockIndex)
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
          val s = newVecInBack(shift, newBlockIndex)
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

          val s = newVecInBack(shift, newBlockIndex)
          s.shiftTopLevel(shiftBlocks, 0) // shift right by n elements
          s.gotoPosWritable(newFocus, newBlockIndex)
          s.display0(32 - shift) = value.asInstanceOf[AnyRef]
          s
        }
      } else {
        val newBlockIndex = blockIndex
        val newFocus = focus

        val s = newVecInBack(0, newBlockIndex)
        s.gotoFreshPosWritable(newFocus, newBlockIndex, newFocus ^ newBlockIndex)
        s.display0(lo) = value.asInstanceOf[AnyRef]
        //assert(s.depth == depth+1) might or might not create new level!
        s
      }
    }
  }

  // low-level implementation (needs cleanup, maybe move to util class)

  private[std] def shiftTopLevel(oldLeft: Int, newLeft: Int) = (depth - 1) match {
    case 0 => display0 = copyRange(display0, oldLeft, newLeft)
    case 1 => display1 = copyRange(display1, oldLeft, newLeft)
    case 2 => display2 = copyRange(display2, oldLeft, newLeft)
    case 3 => display3 = copyRange(display3, oldLeft, newLeft)
    case 4 => display4 = copyRange(display4, oldLeft, newLeft)
    case 5 => display5 = copyRange(display5, oldLeft, newLeft)
  }

  private[std] def zeroLeft(array: Array[AnyRef], index: Int): Unit = {
    var i = 0; while (i < index) { array(i) = null; i+=1 }
  }

  private[std] def zeroRight(array: Array[AnyRef], index: Int): Unit = {
    var i = index; while (i < array.length) { array(i) = null; i+=1 }
  }

  private[std] def preClean(depth: Int) = {
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
  private[std] def cleanLeftEdge(cutIndex: Int) = (levelOf(cutIndex): @switch) match {
    case 0 =>
      zeroLeft(display0, cutIndex)
    case 1 =>
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5))
    case 2 =>
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10))
    case 3 =>
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10) & 0x1f)
      display3 = copyRight(display3, (cutIndex >>> 15))
    case 4 =>
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10) & 0x1f)
      display3 = copyRight(display3, (cutIndex >>> 15) & 0x1f)
      display4 = copyRight(display4, (cutIndex >>> 20))
    case 5 =>
      zeroLeft(display0, cutIndex & 0x1f)
      display1 = copyRight(display1, (cutIndex >>>  5) & 0x1f)
      display2 = copyRight(display2, (cutIndex >>> 10) & 0x1f)
      display3 = copyRight(display3, (cutIndex >>> 15) & 0x1f)
      display4 = copyRight(display4, (cutIndex >>> 20) & 0x1f)
      display5 = copyRight(display5, (cutIndex >>> 25))
    case _ =>
      illegalArgumentException(cutIndex)
  }

  private def copyRight(array: Array[AnyRef], left: Int): Array[AnyRef] =
    new Array[AnyRef](array.length) doto (a2 => arraycopy(array, left, a2, left, a2.length - left))

  private def copyLeft(array: Array[AnyRef], right: Int): Array[AnyRef] =
    new Array[AnyRef](array.length) doto (a2 => arraycopy(array, 0, a2, 0, right))

  // requires structure is writable and at index cutIndex
  private[std] def cleanRightEdge(cutIndex: Int) = {
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

  private[std] def requiredDepth(xor: Int) = {
    if (xor < (1 <<  5)) 1
    else if (xor < (1 << 10)) 2
    else if (xor < (1 << 15)) 3
    else if (xor < (1 << 20)) 4
    else if (xor < (1 << 25)) 5
    else if (xor < (1 << 30)) 6
    else throw new IllegalArgumentException()
  }

  private[std] def dropFront0(cutIndex: Int): Vec[A] = {
    val blockIndex = cutIndex & ~31
    val xor        = cutIndex ^ (endIndex - 1)
    val d          = requiredDepth(xor)
    val shift      = (cutIndex & ~((1 << (5*d))-1))

    // need to init with full display iff going to cutIndex requires swapping block at level >= d
    val s = newVec(cutIndex-shift, endIndex-shift, blockIndex-shift)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex)
    s.preClean(d)
    s.cleanLeftEdge(cutIndex - shift)
    s
  }

  private[std] def dropBack0(cutIndex: Int): Vec[A] = {
    val blockIndex = (cutIndex - 1) & ~31
    val xor = startIndex ^ (cutIndex - 1)
    val d = requiredDepth(xor)
    val shift = (startIndex & ~((1 << (5*d))-1))
    val s = newVec(startIndex-shift, cutIndex-shift, blockIndex-shift)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex)
    s.preClean(d)
    s.cleanRightEdge(cutIndex-shift)
    s
  }
}


final class VectorIterator[@spec(SpecTypes) A](_startIndex: Int, endIndex: Int) extends scIterator[A] with VectorPointer[A @uV] {
  private[std] var blockIndex: Int = _startIndex & ~31
  private[std] var lo: Int         = _startIndex & 31
  private[std] var endLo           = math.min(endIndex - blockIndex, 32)
  private[std] var _hasNext        = blockIndex + lo < endIndex

  def hasNext = _hasNext
  def next(): A = {
    if (!_hasNext) noSuchElementException("reached iterator end")

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
}

sealed trait VectorPointer[@spec(SpecTypes) T] {
  def allocateArray(len: Int): Array[AnyRef] = newArray[AnyRef](len)

  val displays: Displays = new Displays()

  def depth: Int            = displays.depth
  def depth_=(x: Int): Unit = displays.depth = x

  def display0: Array[AnyRef] = displays getDisplay 0
  def display1: Array[AnyRef] = displays getDisplay 1
  def display2: Array[AnyRef] = displays getDisplay 2
  def display3: Array[AnyRef] = displays getDisplay 3
  def display4: Array[AnyRef] = displays getDisplay 4
  def display5: Array[AnyRef] = displays getDisplay 5

  def display0_=(xs: Array[AnyRef]): Unit = displays.setDisplay(0, xs)
  def display1_=(xs: Array[AnyRef]): Unit = displays.setDisplay(1, xs)
  def display2_=(xs: Array[AnyRef]): Unit = displays.setDisplay(2, xs)
  def display3_=(xs: Array[AnyRef]): Unit = displays.setDisplay(3, xs)
  def display4_=(xs: Array[AnyRef]): Unit = displays.setDisplay(4, xs)
  def display5_=(xs: Array[AnyRef]): Unit = displays.setDisplay(5, xs)

  private[std] final def initFrom(that: VectorPointer[T]): VectorPointer[T] = {
    this.depth = that.depth
    displays initFrom that.displays
    this
  }

  // requires structure is at pos oldIndex = xor ^ index
  private[std] final def getElem(index: Int, xor: Int): T = {
    // Should be replaceable with
    //   (displays getElem new Base32(index).asInstanceOf[T]
    // But doesn't quite work somewhere (NPE in Resources test.)

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
  private[std] final def gotoPos(index: Int, xor: Int): Unit = {
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
  private[std] final def gotoNextBlockStart(index: Int, xor: Int): Unit = { // goto block start pos
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
  private[std] final def gotoNextBlockStartWritable(index: Int, xor: Int): Unit = { // goto block start pos
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
  private[std] final def copyOf(a: Array[AnyRef]) =
    new Array[AnyRef](a.length) doto (b => arraycopy(a, 0, b, 0, a.length))

  private[std] final def nullSlotAndCopy(array: Array[AnyRef], index: Int) = {
    val x = array(index)
    array(index) = null
    copyOf(x.asInstanceOf[Array[AnyRef]])
  }

  // make sure there is no aliasing
  // requires structure is at pos index
  // ensures structure is clean and at pos index and writable at all levels except 0

  private[std] final def stabilize(index: Int) = (depth - 1) match {
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
  private[std] final def gotoPosWritable0(newIndex: Int): Unit = (depth - 1) match {
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
  private[std] final def gotoPosWritable1(oldIndex: Int, newIndex: Int): Unit = {
    val xor = oldIndex ^ newIndex

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

  private[std] final def copyRange(array: Array[AnyRef], oldLeft: Int, newLeft: Int) = {
    val elems = allocateArray(32)
    arraycopy(array, oldLeft, elems, newLeft, 32 - math.max(newLeft,oldLeft))
    elems
  }

  // USED IN APPEND
  // create a new block at the bottom level (and possibly nodes on its path) and prepares for writing

  // requires structure is clean and at pos oldIndex,
  // ensures structure is dirty and at pos newIndex and writable at level 0
  private[std] final def gotoFreshPosWritable0(oldIndex: Int, newIndex: Int, xor: Int): Unit = { // goto block start pos
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
