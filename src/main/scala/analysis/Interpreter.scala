package analysis

import cfg._

case class InterpretationException(msg:String) extends Exception

/**
 * An Interpreter for the SSA-CFG format that implicitly defines its semantics.
 */
class Interpreter(fun: Function) extends ValueAnalysis[BigInt](fun) {

  def getVal(v: Value): BigInt = {
    v match {
      case Named(n) => symtab(n) match {
        case Some(x) => x;
        case _ =>
          throw new InterpretationException("Invalid symbol: `" + n +"`!")
      }
      case Const(x) => BigInt(x)
      case _ =>
        throw new InterpretationException("Invalid Value operand: `" + v +"`!")
    }
  }

  def eval(i: Named): Unit = {
    i match {
      case ADD(n, a, b) => symtab(n) = Some(getVal(a) + getVal(b))
      case SUB(n, a, b) => symtab(n) = Some(getVal(a) - getVal(b))
      case MUL(n, a, b) => symtab(n) = Some(getVal(a) * getVal(b))
      case DIV(n, a, b) => symtab(n) = Some(getVal(a) / getVal(b))
      case MOD(n, a, b) => symtab(n) = Some(getVal(a) % getVal(b))
      case SLT(n, a, b) => symtab(n) = Some(if (getVal(a) < getVal(b)) 1 else 0)
      case _ =>
        throw new InterpretationException("Invalid non-PHI Instruction: `"
                                          + i +"`!")
    }
  }

  override def run(): Unit = {
    populateSymbolTable()
    var currBB = fun.First

    val (phis, rest) = currBB.splitPhis



  }

  override def printResult() = {

  }
}

