package scanalyzer.cfg.dominance

import scanalyzer.cfg._
import scanalyzer.analysis.dominance._
import scala.collection.mutable.{Set, Map}

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

class DomTree(fun: Function) {
  def first: DomTreeNode = bbmap(fun.first)
  val bbmap = Map[BasicBlock, DomTreeNode]()

  override def toString(): String = {
    first.toString()
  }
}

object DomTree {
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

  def construct(fun: Function): DomTree = {
    val analysis = new DominanceAnalysis(fun)
    analysis.run()
    constructFromMapping(fun, analysis.getMapping())
  }
}

