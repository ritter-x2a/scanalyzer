package scanalyzer
package cfg

import scala.collection.mutable.{Set, Queue}

sealed class Function(funName: String) extends Iterable[BasicBlock] {
  private val _name: String = funName
  private var _first: BasicBlock = null

  def name: String = _name

  def first: BasicBlock = _first

  def first_=(bb: BasicBlock): Unit = _first = bb

  override def iterator: Iterator[BasicBlock] = new Iterator[BasicBlock] {
    private val visited = Set[BasicBlock]()
    private val queue = new Queue[BasicBlock]() += first

    def hasNext = ! queue.isEmpty

    def next = {
      val res = queue.dequeue
      visited += res
      res foreach {
        case B(cond, ifBB, elseBB) => {
          if (! (visited contains ifBB) && ! (queue contains ifBB)) {
            queue += ifBB
          }
          if (! (visited contains elseBB) && ! (queue contains elseBB)) {
            queue += elseBB
          }
        }
        case _ =>
      }
      res
    }
  }

  /**
   * Traverses all BasicBlocks and applies the given action to each.
   *
   * The essential difference to foreach (via Iterable) is that here, the
   * action is applied BEFORE the successors of the BasicBlock are recorded for
   * the next steps. This is crucial when creating the CFG using dummy target
   * BasicBlocks first and replacing them with this method.
   */
  def traverseBB(action: BasicBlock => Unit): Unit = {
    val visited = Set[BasicBlock]()
    val queue = new Queue[BasicBlock]() += first

    def loop: Unit = {
      val currBB = queue.dequeue

      if (visited contains currBB) {
        return ()
      }

      visited += currBB
      action(currBB)
      currBB foreach {
        case B(cond, ifBB, elseBB) => queue ++= List(ifBB, elseBB)
        case _ =>
      }
    }

    while (! queue.isEmpty)
      loop
  }

  /**
   * Traverses all Instructions and applies the given action to each.
   *
   * This uses the ´traverseBB´ method, consider its documentation for why this
   * is necessary.
   */
  def traverseInstructions(action: Instruction => Unit): Unit =
    traverseBB(bb => bb foreach action)

  override def toString(): String = {
    var res = "fun " + name + " {\n"
    this foreach (bb => res += "" + bb + "\n")
    res += "}\n"
    res
  }
}

