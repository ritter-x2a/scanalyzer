package cfg

/*
 * Inheritance structure:
 *
 *  Instruction      Value
 *    /  |  \       /  |  \
 *   /   |   \     /   |   \
 * RET   B    Named  Const  Undef
 *           /  |  \
 *          /  ...  \
 *        ADD       PHI
 */

/**
 * Super-class for everything 'value-like' in the CFG, e.g. Constants
 * and Instructions.
 */
sealed trait Value {
  def getRepr() : String = {
    this match {
      case Named(n) => n
      case Const(x) => ""+x
      case Undef(n) => "Undef["+n+"]"
      case _ => "[Unsupported Value]"
    }
  }
}

/** A (mathematical) integer Constant. */
case class Const(v: BigInt) extends Value

/** A dummy value that should not be used in any computation. */
case class Undef(n: String) extends Value

/** The possible atomic elements of computation. */
sealed trait Instruction {
  def stringify() : String = {
    def stringifyBinOp(op: String, n: String, a: Value, b: Value): String = {
        ""+ n +" = " + op + " " + a.getRepr() + ", " + b.getRepr()
    }
    this match {
      case ADD(n, a, b) => stringifyBinOp("ADD", n, a, b)
      case SUB(n, a, b) => stringifyBinOp("SUB", n, a, b)
      case MUL(n, a, b) => stringifyBinOp("MUL", n, a, b)
      case DIV(n, a, b) => stringifyBinOp("DIV", n, a, b)
      case MOD(n, a, b) => stringifyBinOp("MOD", n, a, b)
      case SLT(n, a, b) => stringifyBinOp("SLT", n, a, b)
      case B(c, a, b) =>
        "B " + c.getRepr() + ", " + a.Name + ", " + b.Name
      case PHI(n, ops) => {
        var s = ""+ n +" = PHI "
        for ((bb, v) <- ops)
          s = s + " [" + bb.Name + ", "+ v.getRepr() + "]"
        s
      }
      case RET(a) =>
        "RET " + a.getRepr()
      case _ => "[Unsupported Instruction]"
    }
  }
}

case class B(C: Value, TSucc: BasicBlock, FSucc: BasicBlock) extends Instruction
case class RET(Op: Value) extends Instruction

/**
 * Super-class for all named Instructions, i.e. those that yield an actual value.
 */
sealed abstract class Named(n: String) extends Instruction with Value {
  val Name: String = n
}

object Named extends Instruction with Value {
  def unapply(x: Named) = {
    Some(x.Name)
  }
}

case class ADD(N: String, OpA: Value, OpB: Value) extends Named(N)
case class SUB(N: String, OpA: Value, OpB: Value) extends Named(N)
case class MUL(N: String, OpA: Value, OpB: Value) extends Named(N)
case class DIV(N: String, OpA: Value, OpB: Value) extends Named(N)
case class MOD(N: String, OpA: Value, OpB: Value) extends Named(N)
case class SLT(N: String, OpA: Value, OpB: Value) extends Named(N)

case class PHI(N: String,  var Ops: List[(BasicBlock, Value)]) extends Named(N) {
  def getValForBB(bb: BasicBlock): Option[Value] = {
    for ((b, v) <- this.Ops)
      if (b == bb)
        return Some(v)
    return None
  }
}


