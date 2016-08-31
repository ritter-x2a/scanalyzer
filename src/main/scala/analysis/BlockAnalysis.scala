package scanalyzer
package analysis

import cfg._

import scala.collection.mutable.Map

/**
 * Interface for analyses that yield a mapping from BasicBlocks to some
 * values as a result.
 */
abstract class BlockAnalysis[A](fun: Function) extends Analysis {
  protected val blocktab: Map[BasicBlock, A] = Map()

  /**
   * Initializes the internal BasicBlock table with a mapping to init_val for
   * the names of all occuring symbols.
   */
  protected def populateBlockTable(init_val: () => A) = {
    fun foreach {x => blocktab += (x -> init_val())}
  }

  override def getResult(): String = {
    var res = ""
    for ((k, v) <- blocktab)
      res += k.Name + " -> " + v + "\n"
    res
  }
}
