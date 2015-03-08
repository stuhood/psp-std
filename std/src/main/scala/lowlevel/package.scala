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
  /** Here's the bytecode the above produces. We'd like a test which ensures it
   *  stays this way. There is some code in the scala distribution which verifies bytecode
   *  is as expected but it's still not the smoothest process. TODO: test.
   *
   *  In the interim one can inspect the bytecode from "sbt console" via
   *
   *    :javap psp.std.lowlevel.package$
   *
   *  The critical elements are that the only non-local call is the function
   *  application, and the function application is specialized - that is, its name
   *  is "apply$mcVI$sp" and it has a (I)V descriptor, as opposed to one called
   *  "apply" which accepts an Object. Also that the function comes in under 35 bytes,
   *  which is the default hotspot threshold for function inlining.
   *
   *  Preserving those qualities is why the method assumes non-emptiness and has a
   *  slightly unconventional structure. The emptiness check is performed once so can
   *  be lifted outside the method.
   */

  // public final void foreachConsecutive(int, int, scala.Function1<java.lang.Object, scala.runtime.BoxedUnit>);
  //   descriptor: (IILscala/Function1;)V
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
