package cfg

/**
 * Super-class for everything 'value-like' in the CFG, e.g. Constants
 * and Instructions.
 */
sealed abstract class Value {
  def getName() : String = {
    this match {
      case Named(n) => n
      case Const(x) => ""+x
      case Undef() => "Undef"
      case _ => "[Unsupported Value]"
    }
  }
}

/** A (mathematical) integer Constant. */
case class Const(v: BigInt) extends Value

/** A dummy value that should not be used in any computation. */
case class Undef() extends Value

/** The possible atomic elements of computation. */
sealed abstract class Instruction extends Value {
  def stringify() : String = {
    this match {
      case ADD(n, a, b) =>
        ""+ n +" = ADD " + a.getName() + ", " + b.getName()
      case SUB(n, a, b) =>
        ""+ n +" = SUB " + a.getName() + ", " + b.getName()
      case MUL(n, a, b) =>
        ""+ n +" = MUL " + a.getName() + ", " + b.getName()
      case DIV(n, a, b) =>
        ""+ n +" = DIV " + a.getName() + ", " + b.getName()
      case MOD(n, a, b) =>
        ""+ n +" = DIV " + a.getName() + ", " + b.getName()
      case SLT(n, a, b) =>
        ""+ n +" = SLT " + a.getName() + ", " + b.getName()
      case B(c, a, b) =>
        "B " + c.getName() + ", " + a.Name + ", " + b.Name
      case PHI(n, ops) => {
        var s = ""+ n +" = PHI "
        for ((bb, v) <- ops)
          s = s + " [" + bb.Name + ", "+ v.getName() + "]"
        s
      }
      case RET(a) =>
        "RET " + a.getName()
      case _ => "[Unsupported Instruction]"
    }
  }
}

case class B(C: Value, TSucc: BasicBlock, FSucc: BasicBlock) extends Instruction
case class RET(Op: Value) extends Instruction

/**
 * Super-class for all named Instructions, i.e. those that yield an actual value.
 */
sealed abstract class Named(n: String) extends Instruction {
  val Name: String = n
}

object Named extends Instruction {
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


