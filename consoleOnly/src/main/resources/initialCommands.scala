import scala.collection.{ mutable => scm, immutable => sci }
import java.nio.{ file => jnf }
import psp._, std._, api._, pio._, jvm._, cache._
import StdEq._, StdShow._
import psp.std.repl.ReplImport._

def int20  = 1 to 20 map (x => printResult(s"> $x")(x))
