package cfg

sealed class Function(name: String) extends Iterable[BasicBlock]
{
  val Name: String = name
  var First: BasicBlock = null

  override def iterator = new Iterator[BasicBlock] {
    private val visited = collection.mutable.Set[BasicBlock]()
    private var queue = First :: Nil

    def hasNext = ! queue.isEmpty

    def next = {
      val res = queue.head
      visited += res
      queue = queue.tail

      res foreach {
        case B(cond, ifBB, elseBB) =>
          if (! (visited contains elseBB) && ! (queue contains elseBB))
            queue = elseBB :: queue
          if (! (visited contains ifBB) && ! (queue contains ifBB))
            queue = ifBB :: queue
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
    val visited = collection.mutable.Set[BasicBlock]()
    var queue = First :: Nil

    def loop: Unit = {
      val currBB = queue.head
      queue = queue.tail

      if (visited contains currBB)
        return ()

      visited += currBB
      action(currBB)
      currBB foreach {
        case B(cond, ifBB, elseBB) => queue = ifBB :: elseBB :: queue
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
  def traverseInstructions(action: Instruction => Unit) =
    traverseBB(bb => bb foreach action)

  override def toString() = {
    var res = "fun "+name+" {\n"
    this foreach (bb => res += "" + bb + "\n")
    res += "}\n"
    res
  }

  def verify() = {
    // unique names
    // definite assignment?
    // phis only at the beginning of BBs
    // phis have entries for exactly the predecessors?
    // branch/return exactly at BB ends
  }
}

