package scanalyzer
package cfg

/*
 * Inheritance structure:
 *
 *  Instruction      Value
 *    /  |  \       /  |  \
 *   /   |   \     /   |   \
 * RET   B    Named  Const  Undef
 *           /     \
 *          /       \
 *       BinOp      PHI
 */

/**
 * Super-class for everything 'value-like' in the CFG, e.g. Constants
 * and Instructions.
 */
sealed trait Value {
  def getRepr() : String = {
    this match {
      case Named(n) => n
      case Const(x) => "" + x
      case Undef(n) => "Undef[" + n + "]"
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
  val id = InstructionManager.getID()

  var parent: BasicBlock = null

  override def toString() : String = {
    this match {
      case BinOp(n, op, a, b) => {
        val opstr = op match {
          case ADD() => "ADD"
          case SUB() => "SUB"
          case MUL() => "MUL"
          case DIV() => "DIV"
          case SLT() => "SLT"
          case _ => "[Unsupported BinOp]"
        }
        "" + n + " = " + opstr + " " + a.getRepr() + ", " + b.getRepr()
      }
      case PHI(n, ops) => {
        var s = "" + n + " = PHI"
        var first = true
        for ((v, bb) <- ops) {
          if (!first) {
            s += ","
          }
          s = s + " [" + v.getRepr() + ", " + bb.Name + "]"
          first = false
        }
        s
      }
      case B(c, a, b) =>
        "B " + c.getRepr() + ", " + a.Name + ", " + b.Name
      case RET(a) =>
        "RET " + a.getRepr()
      case _ => "[Unsupported Instruction]"
    }
  }
}

object InstructionManager {
  case class IDException(msg:String) extends Exception

  var currentID: Integer = 0

  def getID(): Integer = {
    currentID += 1
    if (currentID == 0) {
      throw new IDException("Instruction IDs overflowed!")
    }
    currentID
  }
}

case class B(var C: Value, var TSucc: BasicBlock, var FSucc: BasicBlock)
  extends Instruction {

  override def hashCode(): Int = this.id

  override def equals(other: Any): Boolean = {
    other match {
      case s: B => this.id == s.id
      case _ => false
    }
  }
}

case class RET(var Op: Value) extends Instruction {

  override def hashCode(): Int = this.id

  override def equals(other: Any): Boolean = {
    other match {
      case s: RET => this.id == s.id
      case _ => false
    }
  }
}

/**
 * Super-class for all named Instructions, i.e. those yielding an actual value.
 */
sealed abstract class Named(n: String) extends Instruction with Value {
  val Name: String = n

  override def hashCode(): Int

  override def equals(other: Any): Boolean
}

object Named extends Instruction with Value {
  def unapply(x: Named): Option[String] =
    Some(x.Name)
}

case class BinOp(N: String, Op: Operator, var OpA: Value, var OpB: Value)
  extends Named(N) {

  override def hashCode(): Int = this.id

  override def equals(other: Any): Boolean = {
    other match {
      case s: BinOp => this.id == s.id
      case _ => false
    }
  }
}


sealed abstract class Operator
case class ADD() extends Operator
case class SUB() extends Operator
case class MUL() extends Operator
case class DIV() extends Operator
case class SLT() extends Operator

case class PHI(N: String,  var Ops: List[(Value, BasicBlock)]) extends Named(N) {
  def getValForBB(bb: BasicBlock): Option[Value] =
    (Ops find (_._2 == bb)) map (_._1)

  override def hashCode(): Int = this.id

  override def equals(other: Any): Boolean = {
    other match {
      case s: PHI => this.id == s.id
      case _ => false
    }
  }
}
