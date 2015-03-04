package psp
package std
package jvm

import JvmOpcodes._

// The start_pc is inclusive and end_pc is exclusive; that is, the exception handler must be active
// while the program counter is within the interval [start_pc, end_pc).
final case class ExceptionHandler(start_pc: UShort, end_pc: UShort, handler_pc: UShort, catch_type: String)

// 4.7.3 The Code Attribute
//
// Code_attribute {
//   u2 attribute_name_index;
//   u4 attribute_length;
//   u2 max_stack;
//   u2 max_locals;
//   u4 code_length;
//   u1 code[code_length];
//   u2 exception_table_length;
//   {   u2 start_pc;
//      u2 end_pc;
//      u2 handler_pc;
//      u2 catch_type;
//   } exception_table[exception_table_length];
//   u2 attributes_count;
//   attribute_info attributes[attributes_count];
// }
case class Code(
      max_stack: UShort,
     max_locals: UShort,
           code: Array[Byte],
exception_table: Array[ExceptionHandler],
     attributes: Array[AttributeInfo]
)(val context: ClassfileModel) extends HasIsEmpty[Code] {
  def isEmpty = this == NoCode
  override def toString = this.opcodesString
}

object Code {
  implicit final class CodeOps(val c: Code) {
    import c._

    def refAt(idx: Int): JvmRef        = context refAt idx
    def nameAt(idx: Int): Lazy[String] = context nameAt idx
    def stringAt(idx: Int): String     = context stringAt idx

    def readInt(idx: Int): Int = (
        (readUByte(idx) << 24)
      | (readUByte(idx + 1) << 16)
      | (readUByte(idx + 2) << 8)
      | (readUByte(idx + 3))
    )
    def readUShort(idx: Int): Int = (
        (readUByte(idx) << 8)
      | (readUByte(idx + 1))
    )
    @inline final def readUByte(idx: Int): Int = code(idx) & 0xFF

    def size = code.length
    lazy val starts: Array[Int] = instructionStarts
    def opcodesString = starts map (idx => "%6d: %s".format(idx, opcodeName(readUByte(idx)))) mkString "\n"
    // lazy val argsSizes = starts :+ starts.length sliding 2 map { x =>
    //   val List(idx1, idx2) = x.toList
    //   idx2 - idx1
    // }

    def isFieldAccess(idx: Int) = readUByte(idx) match {
      case GETSTATIC | PUTSTATIC | GETFIELD | PUTFIELD => true
      case _                                           => false
    }
    def isMethodCall(idx: Int) = readUByte(idx) match {
      case INVOKEVIRTUAL | INVOKESPECIAL | INVOKESTATIC | INVOKEINTERFACE | INVOKEDYNAMIC => true
      case _                                                                              => false
    }
    def opcodeIs(idx: Int, expected: Int) = readUByte(idx) == expected
    //
    // def virtualCalls   = starts collect { case idx if opcodeIs(idx, INVOKEVIRTUAL) => refAt(idx + 1) }
    // def staticCalls    = starts collect { case idx if opcodeIs(idx, INVOKESTATIC) => refAt(idx + 1) }
    // def interfaceCalls = starts collect { case idx if opcodeIs(idx, INVOKEINTERFACE) => refAt(idx + 1) }

    def opcodeUses(code: Int) = starts filter (opcodeIs(_, code))
    def methodCalls   = starts collect { case idx if isMethodCall(idx) => refAt(readUShort(idx + 1)) }
    def fieldAccesses = starts collect { case idx if isFieldAccess(idx) => refAt(readUShort(idx + 1)) }
    def accesses      = methodCalls ++ fieldAccesses

    private def instructionStarts: Array[Int] = {
      var i = 0
      val buf = Array.newBuilder[Int]
      def readUByte  = try this.readUByte(i)  finally i += 1
      def readUShort = try this.readUShort(i) finally i += 2
      def readInt    = try this.readInt(i)    finally i += 4
      def pad()      = i % 4 match {
        case 0  =>
        case x  => i += (4 - x)
      }

      while (i < code.length) {
        val start = i
        buf += start
        val opcode = readUByte

        opcode match {
          case TABLESWITCH     =>
            pad()
            val _ /*default*/, low, high = readInt
            i += (high - low + 1) * 4
          case LOOKUPSWITCH    =>
            pad()
            val _ /*default*/ = readInt
            val numpairs = readInt
            i += (numpairs * 8)
          case MULTIANEWARRAY  =>
            val _ /*elemType*/ = readUShort
            val dimensions     = readUByte
            val sizes          = 0 until dimensions map (_ => readInt)
            sizes
          case WIDE =>
            val instr = readUByte
            i += ( if (instr == IINC_INSN) 4 else 2 )
          case _ =>
            i += opcodeWidth(opcode)
        }
      }
      buf.result
    }
  }
}
