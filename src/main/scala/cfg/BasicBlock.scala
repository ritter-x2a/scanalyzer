package cfg

sealed class BasicBlock(name: String) {
  val Name: String = name
  var Instrs: List[Instruction] = Nil

  override def toString(): String = {
    var res = name + ":\n"
    for (i <- Instrs) {
      res += "  " + i.stringify() + "\n"
    }
    res
  }
}

