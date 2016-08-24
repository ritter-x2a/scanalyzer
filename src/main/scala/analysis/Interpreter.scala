package analysis

import cfg._

case class InterpretationException(msg:String) extends Exception

/**
 * An Interpreter for the SSA-CFG format that implicitly defines its semantics.
 */
class Interpreter(fun: Function) extends ValueAnalysis[Option[BigInt]](fun) {
  def acc[A](a: Option[A]): A = {
    a match {
      case Some(x) => x
      case None =>
        throw new InterpretationException("Use of undefined Value!")
    }
  }

  def eval(i: Named): Unit = {
    i match {
      case ADD(n, a, b) => symtab(n) = Some(acc(getVal(a)) + acc(getVal(b)))
      case SUB(n, a, b) => symtab(n) = Some(acc(getVal(a)) - acc(getVal(b)))
      case MUL(n, a, b) => symtab(n) = Some(acc(getVal(a)) * acc(getVal(b)))
      case DIV(n, a, b) => symtab(n) = Some(acc(getVal(a)) / acc(getVal(b)))
      case MOD(n, a, b) => symtab(n) = Some(acc(getVal(a)) % acc(getVal(b)))
      case SLT(n, a, b) =>
        symtab(n) = Some(if (acc(getVal(a)) < acc(getVal(b))) 1 else 0)
      case _ =>
        throw new InterpretationException("Invalid named non-PHI Instruction: `"
                                          + i +"`!")
    }
  }

  override def fromBigInt(x: BigInt): Option[BigInt] = Some(x)

  override def run(): Unit = {
    populateSymbolTable(None)
    var prevBB: BasicBlock = null
    var currBB: BasicBlock = fun.First

    while (currBB != null) {
      // PHIs are evaluated in parallel
      val (phis, rest) = currBB.splitPhis

      var phi_res: List[Option[BigInt]] = Nil

      for (p <- phis) {
        p.getValForBB(prevBB) match {
          case Some(x) => phi_res = Some(acc(getVal(x))) :: phi_res
          case None =>
            throw new InterpretationException("Insufficient PHI Instruction: `"
                                              + p +"`!")
        }
      }

      phi_res = phi_res.reverse

      for (p <- phis) {
        symtab(p.Name) = phi_res.head
        phi_res = phi_res.tail
      }

      // the non-PHI instructions are evaluated sequentially
      for (i <- rest) {
        i match {
          case B(c, t, f) => {
            prevBB = currBB
            currBB = if (acc(getVal(c)) != 0) t else f
          }
          case RET(x) => {
            symtab("__RES__") = getVal(x)
            prevBB = currBB
            currBB = null
          }
          case x: Named => eval (x)
          case _ => throw new InterpretationException(
            "Invalid unnamed non-PHI Instruction: `" + i +"`!")
        }
      }
    }
  }
}
