package psp
package std

import api._, StdShow._, StdEq._
import Vec._
import java.lang.{ Math => math }

/** Based on the scala library Vector by Tiark Rompf.
 *    https://issues.scala-lang.org/browse/SI-4442
 *    https://issues.scala-lang.org/browse/SI-4495
 *    https://github.com/scala/scala/pull/3498
 */

object Vec {
  type Array0D[A] = A
  type Array1D[A] = Array[A]
  type Array2D[A] = Array[Array[A]]
  type Array3D[A] = Array[Array[Array[A]]]
  type Array4D[A] = Array[Array[Array[Array[A]]]]
  type Array5D[A] = Array[Array[Array[Array[Array[A]]]]]
  type Array6D[A] = Array[Array[Array[Array[Array[Array[A]]]]]]

  final val WIDTH  = 5
  final val BLOCK  = 1 << WIDTH
  final val LOBITS = BLOCK - 1 // masks all but WIDTH bits
  final val HIBITS = ~LOBITS

  def empty[@fspec A] : Vec[A]                        = NIL.castTo[Vec[A]]
  def apply[@fspec A](xs: A*): Vec[A]                 = newBuilder[A] build (Direct scala xs)
  def unapplySeq[@fspec A](x: Vec[A]): Some[scSeq[A]] = Some(x.seq)
  def newBuilder[@fspec A](): Builder[A]              = new Builder[A]()

  private[std] val NIL = new Vec[Any](0, 0, 0)
  // Constants governing concat strategy for performance
  private[std] final val Log2ConcatFaster = 5
  private[std] final val TinyAppendFaster = 2

  @inline final def arr(xs: AnyRef): Array[AnyRef] = xs.asInstanceOf[Array[AnyRef]]

  @inline final def levelBits(index: Int, level: Int): Int =
    index >>> (level * WIDTH) & LOBITS

  def levelOf(index: Int): Int = {
    @tailrec def loop(level: Int, bits: Int): Int =
      if (bits == 0) level else loop(level + 1, bits >>> WIDTH)

    loop(0, index >>> WIDTH)
  }

  final class Builder[@fspec A]() extends Builds[A, Vec[A]] with VectorPointer[A @uV] {
    // possible alternative: start with display0 = null, blockIndex = -BLOCK, lo = BLOCK
    // to avoid allocating initial array if the result will be empty anyways
    this.display0 = new Array[AnyRef](BLOCK)
    this.depth    = 1

    private[this] var blockIndex = 0
    private[this] var lo = 0

    def build(xs: Foreach[A]): Vec[A] = {
      xs foreach add
      result
    }
    def add(elem: A): Unit = {
      if (lo >= display0.length) {
        val newBlockIndex = blockIndex+BLOCK
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

final class Vec[@fspec A](val startIndex: Int, val endIndex: Int, focus: Int) extends VectorPointer[A @uV] with Direct[A] with ShowSelf {
  self =>

  @inline private def preStartIndex = startIndex - 1

  private def nextVec(s: Int, e: Int, f: Int): Vec[A] = new Vec[A](s, e, f)
  private def newVec(s: Int, e: Int, f: Int): Vec[A]  = doto(nextVec(s, e, f))(_ initFrom this)

  private def updateDirty(s: Vec[A]): Vec[A] = { s.dirty = this.dirty ; s }

  private def newVecInFront(shift: Int, newBlockIndex: Int): Vec[A] =
    updateDirty(newVec(preStartIndex + shift, endIndex + shift, newBlockIndex))

  private def newVecInBack(shift: Int, newBlockIndex: Int): Vec[A] =
    updateDirty(newVec(startIndex - shift, endIndex + 1 - shift, newBlockIndex))

  private[std] var dirty = false

  def to_s = "[ " + (this map (_.any_s) mk_s ", ") + " ]"

  def lastIntIndex        = length - 1
  def length: Int         = endIndex - startIndex
  def size: Precise       = Size(length)
  def elemAt(i: Index): A = apply(i.getInt)
  def isEmpty             = length <= 0

  @inline def foreach(f: A => Unit): Unit = {
    if (!isEmpty)
      lowlevel.ll.foreachConsecutive(0, lastIntIndex, i => f(apply(i)))
  }

  def take(n: Index): Vec[A] =
    if (n <= 0) Vec.empty
    else if (n >= length) this
    else dropBack0(startIndex + n.getInt)

  def drop(n: Index): Vec[A] =
    if (n.get <= 0) this
    else if (n >= length) Vec.empty
    else dropFront0(startIndex + n.getInt)

  def takeRight(n: Index): Vec[A] =
    if (n.get <= 0) Vec.empty
    else if (n >= length) this
    else dropFront0(endIndex - n.getInt)

  def dropRight(n: Index): Vec[A] =
    if (n.get <= 0) this
    else if (endIndex - n.getInt > startIndex) dropBack0(endIndex - n.getInt)
    else Vec.empty

  def takeWhile(p: ToBool[A]): Vec[A]      = take(iterator count p)
  def dropWhile(p: ToBool[A]): Vec[A]      = drop(iterator count p)
  def takeRightWhile(p: ToBool[A]): Vec[A] = takeRight(reverseIterator count p)
  def dropRightWhile(p: ToBool[A]): Vec[A] = dropRight(reverseIterator count p)

  @inline def foldl[@fspec B](zero: B)(f: (B, A) => B): B = {
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

  def iterator: VectorIterator[A]                    = initIterator(new VectorIterator[A](startIndex, endIndex))
  def reverseIterator: BiIterator.ReverseIterator[A] = BiIterator reverse this
  def reverse: Vec[A]                                = reverseIterator.toVec
  def apply(index: Int): A                           = getElemWithFocus(checkRangeConvert(index))

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
    s.display0(idx & LOBITS) = elem.asInstanceOf[AnyRef]
    s
  }

  private[std] def appendFront(value: A): Vec[A] = {
    if (endIndex == startIndex)
      return makeSingletonAtBack(value)

    val blockIndex = preStartIndex & HIBITS
    val lo         = preStartIndex & LOBITS

    if (startIndex != blockIndex + BLOCK) {
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
          val newBlockIndex = blockIndex + BLOCK
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
    val elems = allocateArray(BLOCK)
    elems(startIndex) = elem.asInstanceOf[AnyRef]
    val s = nextVec(startIndex, endIndex, 0)
    s.depth = 1
    s.display0 = elems
    s
  }

  private def makeSingletonAtBack(value: A): Vec[A]  = newSingleArray(BLOCK - 1, BLOCK, value)
  private def makeSingletonAtFront(value: A): Vec[A] = newSingleArray(0, 1, value)

  private[std] def appendBack(value: A): Vec[A] = {
    if (endIndex == startIndex)
      return makeSingletonAtFront(value)

    val blockIndex = endIndex & HIBITS
    val lo         = endIndex & LOBITS

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
          val newBlockIndex = blockIndex - BLOCK
          val newFocus = focus

          //assert(newBlockIndex == 0)
          //assert(newFocus == 0)

          val s = newVecInBack(shift, newBlockIndex)
          s.shiftTopLevel(shiftBlocks, 0) // shift right by n elements
          s.gotoPosWritable(newFocus, newBlockIndex)
          s.display0(BLOCK - shift) = value.asInstanceOf[AnyRef]
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
      zeroLeft(display0, cutIndex & LOBITS)
      display1 = copyRight(display1, cutIndex >>>  5)
    case 2 =>
      zeroLeft(display0, cutIndex & LOBITS)
      display1 = copyRight(display1, cutIndex >>>  5 & LOBITS)
      display2 = copyRight(display2, cutIndex >>> 10 & LOBITS)
    case 3 =>
      zeroLeft(display0, cutIndex & LOBITS)
      display1 = copyRight(display1, cutIndex >>>  5 & LOBITS)
      display2 = copyRight(display2, cutIndex >>> 10 & LOBITS)
      display3 = copyRight(display3, cutIndex >>> 15 & LOBITS)
    case 4 =>
      zeroLeft(display0, cutIndex & LOBITS)
      display1 = copyRight(display1, cutIndex >>>  5 & LOBITS)
      display2 = copyRight(display2, cutIndex >>> 10 & LOBITS)
      display3 = copyRight(display3, cutIndex >>> 15 & LOBITS)
      display4 = copyRight(display4, cutIndex >>> 20 & LOBITS)
    case 5 =>
      zeroLeft(display0, cutIndex & LOBITS)
      display1 = copyRight(display1, cutIndex >>>  5 & LOBITS)
      display2 = copyRight(display2, cutIndex >>> 10 & LOBITS)
      display3 = copyRight(display3, cutIndex >>> 15 & LOBITS)
      display4 = copyRight(display4, cutIndex >>> 20 & LOBITS)
      display5 = copyRight(display5, cutIndex >>> 25 & LOBITS)
    case _ =>
      illegalArgumentException(cutIndex)
  }

  private def copyRight(array: Array[AnyRef], left: Int): Array[AnyRef] =
    doto(new Array[AnyRef](array.length))(a2 => arraycopy(array, left, a2, left, a2.length - left))

  private def copyLeft(array: Array[AnyRef], right: Int): Array[AnyRef] =
    doto(new Array[AnyRef](array.length))(a2 => arraycopy(array, 0, a2, 0, right))

  // requires structure is writable and at index cutIndex
  private[std] def cleanRightEdge(cutIndex: Int): Unit = {
    val preCut = cutIndex - 1

    (levelOf(cutIndex): @switch) match {
      // we're actually sitting one block left if cutIndex lies on a block boundary
      // this means that we'll end up erasing the whole block!!
      case 0 =>
        zeroRight(display0, cutIndex)
      case 1 =>
        zeroRight(display0, (preCut & LOBITS) + 1)
        display1 = copyLeft(display1, cutIndex >>>  5)
      case 2 =>
        zeroRight(display0, (preCut & LOBITS) + 1)
        display1 = copyLeft(display1, (preCut >>>  5 & LOBITS) + 1)
        display2 = copyLeft(display2, cutIndex >>> 10)
      case 3 =>
        zeroRight(display0, (preCut & LOBITS) + 1)
        display1 = copyLeft(display1, (preCut >>>  5 & LOBITS) + 1)
        display2 = copyLeft(display2, (preCut >>> 10 & LOBITS) + 1)
        display3 = copyLeft(display3, cutIndex >>> 15)
      case 4 =>
        zeroRight(display0, (preCut & LOBITS) + 1)
        display1 = copyLeft(display1, (preCut >>>  5 & LOBITS) + 1)
        display2 = copyLeft(display2, (preCut >>> 10 & LOBITS) + 1)
        display3 = copyLeft(display3, (preCut >>> 15 & LOBITS) + 1)
        display4 = copyLeft(display4, cutIndex >>> 20)
      case 5 =>
        zeroRight(display0, (preCut & LOBITS) + 1)
        display1 = copyLeft(display1, (preCut >>>  5 & LOBITS) + 1)
        display2 = copyLeft(display2, (preCut >>> 10 & LOBITS) + 1)
        display3 = copyLeft(display3, (preCut >>> 15 & LOBITS) + 1)
        display4 = copyLeft(display4, (preCut >>> 20 & LOBITS) + 1)
        display5 = copyLeft(display5, cutIndex >>> 25)
      case x =>
        illegalArgumentException(x)
    }
  }

  private def requiredDepth(xor: Int): Int = levelOf(xor) + 1

  private[std] def dropFront0(cutIndex: Int): Vec[A] = {
    val blockIndex = cutIndex & HIBITS
    val xor        = cutIndex ^ (endIndex - 1)
    val d          = requiredDepth(xor)
    val shift      = cutIndex & ~((1 << (5*d))-1)

    // need to init with full display iff going to cutIndex requires swapping block at level >= d
    val s = newVec(cutIndex-shift, endIndex-shift, blockIndex-shift)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex)
    s.preClean(d)
    s.cleanLeftEdge(cutIndex - shift)
    s
  }

  private[std] def dropBack0(cutIndex: Int): Vec[A] = {
    val blockIndex = (cutIndex - 1) & HIBITS
    val xor = startIndex ^ (cutIndex - 1)
    val d = requiredDepth(xor)
    val shift = startIndex & ~((1 << 5 * d) - 1)
    val s = newVec(startIndex-shift, cutIndex-shift, blockIndex-shift)
    s.dirty = dirty
    s.gotoPosWritable(focus, blockIndex)
    s.preClean(d)
    s.cleanRightEdge(cutIndex-shift)
    s
  }
}


final class VectorIterator[@fspec A](_startIndex: Int, endIndex: Int) extends scIterator[A] with VectorPointer[A @uV] {
  private[std] var blockIndex: Int = _startIndex & HIBITS
  private[std] var lo: Int         = _startIndex & LOBITS
  private[std] var endLo           = math.min(endIndex - blockIndex, BLOCK)
  private[std] var _hasNext        = blockIndex + lo < endIndex

  def hasNext = _hasNext
  def next(): A = {
    if (!_hasNext) noSuchElementException("reached iterator end")

    val res = display0(lo).asInstanceOf[A]
    lo += 1

    if (lo == endLo) {
      if (blockIndex + lo < endIndex) {
        val newBlockIndex = blockIndex+BLOCK
        gotoNextBlockStart(newBlockIndex, blockIndex ^ newBlockIndex)

        blockIndex = newBlockIndex
        endLo = math.min(endIndex - blockIndex, BLOCK)
        lo = 0
      } else {
        _hasNext = false
      }
    }

    res
  }
}

sealed trait VectorPointer[@fspec T] {
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

  private def ensure0(): Unit = display0 = new Array(BLOCK)
  private def ensure1(xs: AnyRef): Unit = xs match {
    case null => display1 = new Array(BLOCK)
    case _    => display1 = arr(xs)
  }
  private def ensure2(xs: AnyRef): Unit = xs match {
    case null => display2 = new Array(BLOCK)
    case _    => display2 = arr(xs)
  }
  private def ensure3(xs: AnyRef): Unit = xs match {
    case null => display3 = new Array(BLOCK)
    case _    => display3 = arr(xs)
  }
  private def ensure4(xs: AnyRef): Unit = xs match {
    case null => display4 = new Array(BLOCK)
    case _    => display4 = arr(xs)
  }

  private[std] final def initFrom(that: VectorPointer[T]): VectorPointer[T] = {
    this.depth = that.depth
    displays initFrom that.displays
    this
  }

  // requires structure is at pos oldIndex = xor ^ index
  private[std] final def getElem(index: Int, xor: Int): T = (levelOf(xor): @switch) match {
    case 0 => display0(index & LOBITS).asInstanceOf[T]
    case 1 => display1(index >> 5 & LOBITS).asInstanceOf[Array[AnyRef]](index & LOBITS).asInstanceOf[T]
    case 2 => display2(index >> 10 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 5 & LOBITS).asInstanceOf[Array[AnyRef]](index & LOBITS).asInstanceOf[T]
    case 3 => display3(index >> 15 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 10 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 5 & LOBITS).asInstanceOf[Array[AnyRef]](index & LOBITS).asInstanceOf[T]
    case 4 => display4(index >> 20 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 15 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 10 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 5 & LOBITS).asInstanceOf[Array[AnyRef]](index & LOBITS).asInstanceOf[T]
    case 5 => display5(index >> 25 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 20 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 15 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 10 & LOBITS).asInstanceOf[Array[AnyRef]](index >> 5 & LOBITS).asInstanceOf[Array[AnyRef]](index & LOBITS).asInstanceOf[T]
    case _ => illegalArgumentException(xor)
  }

  // go to specific position
  // requires structure is at pos oldIndex = xor ^ index,
  // ensures structure is at pos index
  private[std] final def gotoPos(index: Int, xor: Int): Unit = (levelOf(xor): @switch) match {
    case 0 =>
    case 1 =>
      display0 = arr(display1(index >> 5 & LOBITS))
    case 2 =>
      display1 = arr(display2(index >> 10 & LOBITS))
      display0 = arr(display1(index >>  5 & LOBITS))
    case 3 =>
      display2 = arr(display3(index >> 15 & LOBITS))
      display1 = arr(display2(index >> 10 & LOBITS))
      display0 = arr(display1(index >>  5 & LOBITS))
    case 4 =>
      display3 = arr(display4(index >> 20 & LOBITS))
      display2 = arr(display3(index >> 15 & LOBITS))
      display1 = arr(display2(index >> 10 & LOBITS))
      display0 = arr(display1(index >>  5 & LOBITS))
    case 5 =>
      display4 = arr(display5(index >> 25 & LOBITS))
      display3 = arr(display4(index >> 20 & LOBITS))
      display2 = arr(display3(index >> 15 & LOBITS))
      display1 = arr(display2(index >> 10 & LOBITS))
      display0 = arr(display1(index >>  5 & LOBITS))
    case _ =>
      illegalArgumentException(xor)
  }

  // USED BY ITERATOR

  // xor: oldIndex ^ index
  // goto block start pos
  private[std] final def gotoNextBlockStart(index: Int, xor: Int): Unit = (levelOf(xor): @switch) match {
    case 0 | 1 =>
      display0 = arr(display1(index >> 5 & LOBITS))
    case 2 =>
      display1 = arr(display2(index >> 10 & LOBITS))
      display0 = arr(display1(0))
    case 3 =>
      display2 = arr(display3(index >> 15 & LOBITS))
      display1 = arr(display2(0))
      display0 = arr(display1(0))
    case 4 =>
      display3 = arr(display4(index >> 20 & LOBITS))
      display2 = arr(display3(0))
      display1 = arr(display2(0))
      display0 = arr(display1(0))
    case 5 =>
      display4 = arr(display5(index >> 25 & LOBITS))
      display3 = arr(display4(0))
      display2 = arr(display3(0))
      display1 = arr(display2(0))
      display0 = arr(display1(0))
    case _ =>
      illegalArgumentException(xor)
  }

  // USED BY BUILDER

  // xor: oldIndex ^ index
  // goto block start pos
  private[std] final def gotoNextBlockStartWritable(index: Int, xor: Int): Unit = (levelOf(xor): @switch) match {
    case 0 | 1 =>
      if (depth == 1) { display1 = new Array(BLOCK); display1(0) = display0; depth+=1}
      display0 = new Array(BLOCK)
      display1(index >>  5 & LOBITS) = display0
    case 2 =>
      if (depth == 2) { display2 = new Array(BLOCK); display2(0) = display1; depth+=1}
      display0 = new Array(BLOCK)
      display1 = new Array(BLOCK)
      display1(index >>  5 & LOBITS) = display0
      display2(index >> 10 & LOBITS) = display1
    case 3 =>
      if (depth == 3) { display3 = new Array(BLOCK); display3(0) = display2; depth+=1}
      display0 = new Array(BLOCK)
      display1 = new Array(BLOCK)
      display2 = new Array(BLOCK)
      display1(index >>  5 & LOBITS) = display0
      display2(index >> 10 & LOBITS) = display1
      display3(index >> 15 & LOBITS) = display2
    case 4 =>
      if (depth == 4) { display4 = new Array(BLOCK); display4(0) = display3; depth+=1}
      display0 = new Array(BLOCK)
      display1 = new Array(BLOCK)
      display2 = new Array(BLOCK)
      display3 = new Array(BLOCK)
      display1(index >>  5 & LOBITS) = display0
      display2(index >> 10 & LOBITS) = display1
      display3(index >> 15 & LOBITS) = display2
      display4(index >> 20 & LOBITS) = display3
    case 5 =>
      if (depth == 5) { display5 = new Array(BLOCK); display5(0) = display4; depth+=1}
      display0 = new Array(BLOCK)
      display1 = new Array(BLOCK)
      display2 = new Array(BLOCK)
      display3 = new Array(BLOCK)
      display4 = new Array(BLOCK)
      display1(index >>  5 & LOBITS) = display0
      display2(index >> 10 & LOBITS) = display1
      display3(index >> 15 & LOBITS) = display2
      display4(index >> 20 & LOBITS) = display3
      display5(index >> 25 & LOBITS) = display4
    case _ =>
      illegalArgumentException(xor)
  }

  // STUFF BELOW USED BY APPEND / UPDATE
  private[std] final def copyOf(a: Array[AnyRef]) =
    doto(new Array[AnyRef](a.length))(b => arraycopy(a, 0, b, 0, a.length))

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
      display5(index >> 25 & LOBITS) = display4
      display4(index >> 20 & LOBITS) = display3
      display3(index >> 15 & LOBITS) = display2
      display2(index >> 10 & LOBITS) = display1
      display1(index >>  5 & LOBITS) = display0
    case 4 =>
      display4 = copyOf(display4)
      display3 = copyOf(display3)
      display2 = copyOf(display2)
      display1 = copyOf(display1)
      display4(index >> 20 & LOBITS) = display3
      display3(index >> 15 & LOBITS) = display2
      display2(index >> 10 & LOBITS) = display1
      display1(index >>  5 & LOBITS) = display0
    case 3 =>
      display3 = copyOf(display3)
      display2 = copyOf(display2)
      display1 = copyOf(display1)
      display3(index >> 15 & LOBITS) = display2
      display2(index >> 10 & LOBITS) = display1
      display1(index >>  5 & LOBITS) = display0
    case 2 =>
      display2 = copyOf(display2)
      display1 = copyOf(display1)
      display2(index >> 10 & LOBITS) = display1
      display1(index >>  5 & LOBITS) = display0
    case 1 =>
      display1 = copyOf(display1)
      display1(index >>  5 & LOBITS) = display0
    case 0 =>
  }

  /// USED IN UPDATE AND APPEND BACK

  // prepare for writing at an existing position

  // requires structure is clean and at pos oldIndex = xor ^ newIndex,
  // ensures structure is dirty and at pos newIndex and writable at level 0
  private[std] final def gotoPosWritable0(newIndex: Int): Unit = (depth - 1) match {
    case 5 =>
      display5 = copyOf(display5)
      display4 = arr(nullSlotAndCopy(display5, newIndex >> 25 & LOBITS))
      display3 = arr(nullSlotAndCopy(display4, newIndex >> 20 & LOBITS))
      display2 = arr(nullSlotAndCopy(display3, newIndex >> 15 & LOBITS))
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 4 =>
      display4 = copyOf(display4)
      display3 = arr(nullSlotAndCopy(display4, newIndex >> 20 & LOBITS))
      display2 = arr(nullSlotAndCopy(display3, newIndex >> 15 & LOBITS))
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 3 =>
      display3 = copyOf(display3)
      display2 = arr(nullSlotAndCopy(display3, newIndex >> 15 & LOBITS))
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 2 =>
      display2 = copyOf(display2)
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 1 =>
      display1 = copyOf(display1)
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 0 =>
      display0 = copyOf(display0)
  }


  // requires structure is dirty and at pos oldIndex,
  // ensures structure is dirty and at pos newIndex and writable at level 0
  private[std] final def gotoPosWritable1(oldIndex: Int, newIndex: Int): Unit = (levelOf(oldIndex ^ newIndex): @switch) match {
    case 0 =>
      display0 = copyOf(display0)
    case 1 =>
      display1 = copyOf(display1)
      display1(oldIndex >> 5 & LOBITS) = display0
      display0 = nullSlotAndCopy(display1, newIndex >>  5 & LOBITS)
    case 2 =>
      display1 = copyOf(display1)
      display2 = copyOf(display2)
      display1(oldIndex >>  5 & LOBITS) = display0
      display2(oldIndex >> 10 & LOBITS) = display1
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 3 =>
      display1 = copyOf(display1)
      display2 = copyOf(display2)
      display3 = copyOf(display3)
      display1(oldIndex >>  5 & LOBITS) = display0
      display2(oldIndex >> 10 & LOBITS) = display1
      display3(oldIndex >> 15 & LOBITS) = display2
      display2 = arr(nullSlotAndCopy(display3, newIndex >> 15 & LOBITS))
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 4 =>
      display1 = copyOf(display1)
      display2 = copyOf(display2)
      display3 = copyOf(display3)
      display4 = copyOf(display4)
      display1(oldIndex >>  5 & LOBITS) = display0
      display2(oldIndex >> 10 & LOBITS) = display1
      display3(oldIndex >> 15 & LOBITS) = display2
      display4(oldIndex >> 20 & LOBITS) = display3
      display3 = arr(nullSlotAndCopy(display4, newIndex >> 20 & LOBITS))
      display2 = arr(nullSlotAndCopy(display3, newIndex >> 15 & LOBITS))
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case 5 =>
      display1 = copyOf(display1)
      display2 = copyOf(display2)
      display3 = copyOf(display3)
      display4 = copyOf(display4)
      display5 = copyOf(display5)
      display1(oldIndex >>  5 & LOBITS) = display0
      display2(oldIndex >> 10 & LOBITS) = display1
      display3(oldIndex >> 15 & LOBITS) = display2
      display4(oldIndex >> 20 & LOBITS) = display3
      display5(oldIndex >> 25 & LOBITS) = display4
      display4 = arr(nullSlotAndCopy(display5, newIndex >> 25 & LOBITS))
      display3 = arr(nullSlotAndCopy(display4, newIndex >> 20 & LOBITS))
      display2 = arr(nullSlotAndCopy(display3, newIndex >> 15 & LOBITS))
      display1 = arr(nullSlotAndCopy(display2, newIndex >> 10 & LOBITS))
      display0 = arr(nullSlotAndCopy(display1, newIndex >>  5 & LOBITS))
    case x =>
      illegalArgumentException(x)
  }

  // USED IN DROP

  private[std] final def copyRange(array: Array[AnyRef], oldLeft: Int, newLeft: Int) = {
    val elems = allocateArray(BLOCK)
    arraycopy(array, oldLeft, elems, newLeft, BLOCK - math.max(newLeft,oldLeft))
    elems
  }

  // USED IN APPEND
  // create a new block at the bottom level (and possibly nodes on its path) and prepares for writing

  // requires structure is clean and at pos oldIndex,
  // ensures structure is dirty and at pos newIndex and writable at level 0
  // goto block start pos
  private[std] final def gotoFreshPosWritable0(oldIndex: Int, newIndex: Int, xor: Int): Unit = (levelOf(xor): @switch) match {
    case 0 => ()
    case 1 =>
      if (depth == 1) {
        display1 = new Array(BLOCK)
        display1(oldIndex >>  5 & LOBITS) = display0
        depth +=1
      }
      ensure0()
    case 2 =>
      if (depth == 2) {
        display2 = new Array(BLOCK)
        display2(oldIndex >> 10 & LOBITS) = display1
        depth +=1
      }
      ensure1(display2(newIndex >> 10 & LOBITS))
      ensure0()
    case 3 =>
      if (depth == 3) {
        display3 = new Array(BLOCK)
        display3(oldIndex >> 15 & LOBITS) = display2
        depth +=1
      }
      ensure2(display3(newIndex >> 15 & LOBITS))
      ensure1(display2(newIndex >> 10 & LOBITS))
      ensure0()
    case 4 =>
      if (depth == 4) {
        display4 = new Array(BLOCK)
        display4(oldIndex >> 20 & LOBITS) = display3
        depth +=1
      }
      ensure3(display4(newIndex >> 20 & LOBITS))
      ensure2(display3(newIndex >> 15 & LOBITS))
      ensure1(display2(newIndex >> 10 & LOBITS))
      ensure0()
    case 5 =>
      if (depth == 5) {
        display5 = new Array(BLOCK)
        display5(oldIndex >>  25 & LOBITS) = display4
        depth +=1
      }
      ensure4(display5(newIndex >> 25 & LOBITS))
      ensure3(display4(newIndex >> 20 & LOBITS))
      ensure2(display3(newIndex >> 15 & LOBITS))
      ensure1(display2(newIndex >> 10 & LOBITS))
      ensure0()
    case x =>
      illegalArgumentException(x)
  }
}
