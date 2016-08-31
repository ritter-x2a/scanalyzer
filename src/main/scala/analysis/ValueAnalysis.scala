package scanalyzer
package analysis

import cfg._

import scala.collection.mutable.Map

/**
 * General interface for abstract values.
 */
trait AbstractVal[A <: AbstractVal[A]] { this: A =>
  def join(other: A): A

  def add(other: A): A
  def sub(other: A): A
  def mul(other: A): A
  def div(other: A): A

  def slt(other: A): A

  def canBeZero(): Boolean
  def canBeNonZero(): Boolean
}

/**
 * Interface for analyses that yield a mapping from occuring symbols to some
 * values as a result.
 */
abstract class ValueAnalysis[A](fun: Function) extends Analysis {
  protected val symtab: Map[String, A] = Map()

  /**
   * Initializes the internal symbol table with a mapping to init_val for the
   * names of all occuring symbols.
   */
  protected def populateSymbolTable(init_val: A) = {
    fun.traverseInstructions {
      case Named(n) if !(symtab contains n) => symtab += (n -> init_val)
      case _ =>
    }
  }

  /**
   * Returns an AbstractVal for a given SSA Value.
   *
   * This is either its entry in the symbol table (if it is a named
   * instruction) or the representative of a BigInt constant (if the Value is a
   * Const).
   */
  protected def getVal(v: Value): A = {
    v match {
      case Named(n) => symtab(n)
      case Const(x) => fromBigInt(x)
      case _ =>
        throw new AnalysisException("Invalid Value operand: `" + v + "`!")
    }
  }

  /**
   * Constructs an AbstractVal from a BigInt constant.
   */
  protected def fromBigInt(x: BigInt): A

  override def getResult(): String = {
    var res = ""
    for ((k, v) <- symtab)
      res += k + " -> " + v + "\n"
    res
  }
}

