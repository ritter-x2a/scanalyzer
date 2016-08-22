package cfg

sealed class Function(name: String) {
  val Name: String = name
  var First: BasicBlock = null

  def traverseInstructions(action: Instruction => Unit) = {
    val visited: collection.mutable.Set[BasicBlock] = collection.mutable.Set(First)
    var queue = First :: Nil

    def traverseBB(): Unit = {
      val currBB = queue.head
      queue = queue.tail

      if (visited contains currBB)
        return ()

      visited += currBB

      for (instr <- currBB.Instrs) {
        action(instr)
        instr match {
          case B(cond, ifBB, elseBB) =>
            queue = ifBB :: elseBB :: queue
          case _ => ;
        }
      }
    }

    while (! queue.isEmpty) {
      traverseBB()
    }
  }

  // def verify() = {...}
}

