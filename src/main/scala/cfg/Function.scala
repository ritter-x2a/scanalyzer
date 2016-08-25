package cfg

sealed class Function(name: String) {
  val Name: String = name
  var First: BasicBlock = null


  def traverseBB(action: BasicBlock => Unit): Unit = {
    val visited: collection.mutable.Set[BasicBlock] = collection.mutable.Set()
    var queue = First :: Nil

    while (! queue.isEmpty) {
      val currBB = queue.head
      queue = queue.tail

      if (visited contains currBB)
        return ()

      visited += currBB

      action(currBB)

      for (instr <- currBB.Instrs) {
        instr match {
          case B(cond, ifBB, elseBB) =>
            queue = ifBB :: elseBB :: queue
          case _ => ;
        }
      }
    }
  }

  def traverseInstructions(action: Instruction => Unit) = {
    traverseBB((bb: BasicBlock) => {
      for (instr <- bb.Instrs)
        action(instr)
    })
  }

  def print() = {
    traverseBB((bb: BasicBlock) =>
      println(bb.toString)
    )
  }

  def verify() = {
    // unique names
    // definite assignment?
    // phis only at the beginning of BBs
    // phis have entries for exactly the predecessors?
    // branch/return exactly at BB ends
  }
}

