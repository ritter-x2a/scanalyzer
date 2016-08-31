package scanalyzer
package analysis
package interpreter

import cfg._
import util._

case class InterpretationException(msg:String) extends ScanalyzerException(msg)

/**
 * An Interpreter for the SSA-CFG format that implicitly defines its semantics.
 */
class Interpreter(fun: Function) extends ValueAnalysis[Option[BigInt]](fun) {
  private def err(msg: String) = throw new InterpretationException(msg)

  private def acc[A](a: Option[A]): A =
    a.getOrElse(err("Use of undefined Value!"))

  private def eval(i: Named): Unit = {
    i match {
      case BinOp(n, op, a, b) => {
        val aval = acc(getVal(a))
        val bval = acc(getVal(b))
        symtab(n) = Some(op match {
          case ADD() => aval + bval
          case SUB() => aval - bval
          case MUL() => aval * bval
          case DIV() => if (bval != 0) {
            aval / bval
          } else {
            err("Division by zero!")
          }
          case SLT() => if (aval < bval) 1 else 0
        })
      }
      case _ => err("Invalid named non-PHI Instruction: `" + i + "`!")
    }
  }

  override protected def fromBigInt(x: BigInt) = Some(x)

  override def run(): Unit = {
    populateSymbolTable(None)
    var prevBB: BasicBlock = null
    var currBB: BasicBlock = fun.first

    while (currBB != null) {
      // PHIs are evaluated in parallel
      val (phis, rest) = currBB.splitPhis

      // collect the appropriate values from all PHIs
      var phi_res = phis map (p => {
        p.getValForBB(prevBB) match {
          case Some(x) => Some(acc(getVal(x)))
          case None => err("Insufficient PHI Instruction: `" + p +"`!")
        }
      })

      // assign the new values
      for ((p, r) <- phis zip phi_res)
        symtab(p.Name) = r

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
          case _ => err( "Invalid unnamed non-PHI Instruction: `" + i + "`!")
        }
      }
    }
  }
}
