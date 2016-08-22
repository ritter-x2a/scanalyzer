package cfg

sealed abstract class Value {
  def getName() : String = {
    this match {
      case ADD(n, _, _) => n
      case SLT(n, _, _) => n
      case PHI(n, _) => n

      case Const(x) => ""+x
      case Undef() => "Undef"
      case _ => "[Unsupported Value]"
    }
  }
}

case class Const(v: Int) extends Value
case class Undef() extends Value

sealed abstract class Instruction extends Value {
  def stringify() : String = {
    this match {
      case ADD(n, a, b) =>
        ""+ n +" = ADD " + a.getName() + ", " + b.getName()
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

case class B(Cond: Value, TrueSucc: BasicBlock, FalseSucc: BasicBlock) extends Instruction
case class RET(Op: Value) extends Instruction

case class Named(Name: String) extends Instruction
case class ADD(Name: String, OpA: Value, OpB: Value) extends Named(Name)
case class SLT(Name: String, OpA: Value, OpB: Value) extends Named(Name)
case class PHI(Name: String,  var Ops: List[(BasicBlock, Value)]) extends Named(Name)

