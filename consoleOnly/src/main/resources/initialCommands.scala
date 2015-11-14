import scala.collection.{ mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp._, std._, api._, pio._, jvm._, repl._
import StdEq._, StdShow._

def int20: View[Int] = 1 to 20 tee ("> " + _)
