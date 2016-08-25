package analysis

import cfg._
import scala.collection.mutable.Map

case class AnalysisException(msg:String) extends Exception

/**
 * General interface for analyses that work on function CFGs.
 */
trait Analysis {
  def run(): Unit
  def getResult(): String
}

/**
 * Interface for analyses that yield a mapping from occuring symbols to some
 * values as a result.
 */
abstract class ValueAnalysis[A](fun: Function) extends Analysis {
  protected val symtab: Map[String, A] = Map()

  /**
   * Initialize the internal symbol table with a mapping to init_val for the
   * names of all occuring symbols.
   */
  protected def populateSymbolTable(init_val: A) = {
    fun.traverseInstructions {
      case Named(n) if !(symtab contains n) => symtab += (n -> init_val)
      case _ =>
    }
  }

  protected def getVal(v: Value): A = {
    v match {
      case Named(n) => symtab(n)
      case Const(x) => fromBigInt(x)
      case _ =>
        throw new AnalysisException("Invalid Value operand: `" + v + "`!")
    }
  }

  protected def fromBigInt(x: BigInt): A

  override def getResult(): String = {
    var res = ""
    for ((k, v) <- symtab)
      res += k + " -> " + v + "\n"
    res
  }
}
