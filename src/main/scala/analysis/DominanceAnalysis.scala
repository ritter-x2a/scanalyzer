package scanalyzer.analysis.dominance

import scanalyzer.analysis._
import scanalyzer.cfg._
import scanalyzer.util._
import scala.collection.mutable.{Set, Map, Queue}

class DominanceAnalysis(fun: Function)
  extends BlockAnalysis[Set[BasicBlock]](fun) {


  override def run(): Unit = {
    val allBlocks = Set[BasicBlock]()
    fun foreach ( bb => allBlocks += bb )

    populateBlockTable(allBlocks.clone)
    blocktab(fun.first) = Set[BasicBlock](fun.first)

    val queue = new Queue[BasicBlock]

    queue ++= allBlocks

    while (! queue.isEmpty) {
      val current = queue.dequeue
      val value = blocktab(current)
      val succs = current.getSuccessors()

      succs foreach (bb => {
        val entry = blocktab(bb)
        val sizeBefore = entry.size
        entry retain (b =>  b == current || (value contains b))
        if (sizeBefore != entry.size && ! (queue contains bb)) {
          queue += bb
        }
      })
    }
  }

  override def getResult(): String = {
    var res = ""
    for ((k, v) <- blocktab)
      res += k.Name + " -> " + Util.strItBB(v) + "\n"
    res
  }
}
