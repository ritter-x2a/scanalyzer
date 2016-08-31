package scanalyzer
package analysis
package dominance

import cfg._

import scala.collection.mutable.{Set, Map}

/**
 * Representation of a node in the dominance tree.
 *
 * Every node wraps a BasicBlock.
 */
class DomTreeNode(bb: BasicBlock) {
  var children: List[DomTreeNode] = Nil

  private def stringify(prefix: String): String = {
    var res: String = prefix
    res = res + "- " + (bb.Name + "\n")
    children foreach (n => res = res + n.stringify(prefix + "  "))
    res
  }

  override def toString(): String = {
    stringify("")
  }
}

/**
 * The representation of a dominance tree.
 *
 * BasicBlocks are ordered by dominance.
 */
class DomTree(fun: Function) {
  def first: DomTreeNode = bbmap(fun.first)
  val bbmap = Map[BasicBlock, DomTreeNode]()

  /**
   * Returns `true` iff every use of a value is dominated by its definition.
   */
  def verifySSA(): Boolean = {
    // TODO
    true
  }

  override def toString(): String = {
    first.toString()
  }
}

/**
 * Companion object for constructor functions.
 */
object DomTree {

  /**
   *  Construct a DomTree from a map that maps Dominators to BasicBlocks.
   *  Such a map can be obtained from the DominanceAnalysis.
   */
  def constructFromMapping(
    fun: Function,
    domMap: Map[BasicBlock, Set[BasicBlock]]
  ): DomTree = {

    val res = new DomTree(fun)

    fun foreach (bb => res.bbmap += (bb -> new DomTreeNode(bb)))

    fun foreach (bb => {
      domMap(bb) -= bb
      if (domMap(bb).size > 0) {
        val parentBB = domMap(bb).reduceLeft((a, b) => {
          if (domMap(a).size < domMap(b).size) b else a
        })
        val parent = res.bbmap(parentBB)
        parent.children = res.bbmap(bb) :: parent.children
      }
    })
    res
  }

  /**
   * Construct the DomTree for a function. Uses DominanceAnalysis.
   */
  def construct(fun: Function): DomTree = {
    val analysis = new DominanceAnalysis(fun)
    analysis.run()
    constructFromMapping(fun, analysis.getMapping())
  }
}

