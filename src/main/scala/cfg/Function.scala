package cfg

sealed class Function(name: String) extends Iterable[BasicBlock] {
  val Name: String = name
  var First: BasicBlock = null

  override def iterator = new Iterator[BasicBlock] {
    val visited: collection.mutable.Set[BasicBlock] = collection.mutable.Set()
    var queue = First :: Nil

    def hasNext = ! queue.isEmpty

    def next = {
      val res = queue.head
      visited += res
      queue = queue.tail

      for (instr <- res.Instrs) {
        instr match {
          case B(cond, ifBB, elseBB) =>
            if (! (visited contains elseBB) && ! (queue contains elseBB))
              queue = elseBB :: queue
            if (! (visited contains ifBB) && ! (queue contains ifBB))
              queue = ifBB :: queue
          case _ => ;
        }
      }

      res
    }
  }

  def traverseBB(action: BasicBlock => Unit): Unit = {
    val visited: collection.mutable.Set[BasicBlock] = collection.mutable.Set()
    var queue = First :: Nil

    def loop: Unit = {
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

    while (! queue.isEmpty)
      loop
  }

  def traverseInstructions(action: Instruction => Unit) = {
    traverseBB((bb: BasicBlock) => {
      for (instr <- bb.Instrs)
        action(instr)
    })
  }

  override def toString() = {
    var res = "fun "+name+" {\n"
    map((bb: BasicBlock) =>
      res += bb.toString + "\n"
    )
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

