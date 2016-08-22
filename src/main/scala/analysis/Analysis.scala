package analysis

import cfg._
import scala.collection.mutable.Map

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
  val symtab: Map[String, Option[T]] = Map()

  /**
   * Initialize the internal symbol table with a mapping to none (=> "Bottom")
   * for the names of all occuring symbols.
   */
  def populateSymbolTable() = {
    fun.traverseInstructions((i: Instruction) => {
      i match {
        case Named(n) if !(symtab contains n) =>
            symtab += (n -> None)
        case _ => None
      }
    })
  }
}
