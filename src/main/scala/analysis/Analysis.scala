package analysis

import cfg._
import scala.collection.mutable.Map

case class AnalysisException(msg:String) extends Exception

/**
 * General interface for analyses that work on function CFGs.
 */
trait Analysis {
  def run(): Unit
  def printResult(): Unit
}

/**
 * Interface for analyses that yield a mapping from occuring symbols to some
 * values as a result.
 */
abstract class ValueAnalysis[T](fun: Function) extends Analysis {
  val symtab: Map[String, T] = Map()

  /**
   * Initialize the internal symbol table with a mapping to none (=> "Bottom")
   * for the names of all occuring symbols.
   */
  def populateSymbolTable(init_val: T) = {
    fun.traverseInstructions((i: Instruction) => {
      i match {
        case Named(n) if !(symtab contains n) =>
            symtab += (n -> init_val)
        case _ => ()
      }
    })
  }

  def getVal(v: Value): T = {
    v match {
      case Named(n) => symtab(n)
      case Const(x) => fromBigInt(x)
      case _ =>
        throw new AnalysisException("Invalid Value operand: `" + v +"`!")
    }
  }

  def fromBigInt(x: BigInt): T

  override def printResult() = {
    for ((k, v) <- symtab)
      println(k+ " -> " + v)
  }
}
