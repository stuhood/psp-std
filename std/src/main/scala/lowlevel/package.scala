package psp
package std

package object lowlevel {
  /** Can't refer directly to fields because scala bloats all the bytecode
   *  going through getters. This way the parameters are locals.
   *  @pre non-empty sequence
   *  @param start    the first int
   *  @param last     the last int
   *  @param f        the function to apply
   */
  @inline final def foreachConsecutive(start: Int, last: Int, f: Int => Unit): Unit = {
    var elem = start - 1
    while (true) {
      elem += 1
      f(elem)
      if (elem == last) return
    }
  }
  // public final void foreachInt(int, int, scala.Function1<java.lang.Object, scala.runtime.BoxedUnit>);
  //        0: iload_1
  //        1: iconst_1
  //        2: isub
  //        3: istore        4
  //        5: iload         4
  //        7: iconst_1
  //        8: iadd
  //        9: istore        4
  //       11: aload_3
  //       12: iload         4
  //       14: invokeinterface #20,  2           // InterfaceMethod scala/Function1.apply$mcVI$sp:(I)V
  //       19: iload         4
  //       21: iload_2
  //       22: if_icmpne     5
  //       25: return
}
