package cfg

/**
 * A structure containing a straight-line sequence of instructions.
 *
 * All PHI Instructions have to be leading in the BasicBlock.
 * There should be exactly one branch/return Instruction in a BasicBlock,
 * namely the last Instruction.
 */
sealed class BasicBlock(name: String) extends Iterable[Instruction] {
  val Name: String = name
  var Instrs: List[Instruction] = Nil

  override def iterator: Iterator[Instruction] = Instrs.iterator

  /**
   * Returns two lists, one with the PHIs Instructions of the BasicBlock and
   * one with the remaining non-PHI Instructions.
   *
   * Notice that the order of the PHI instructions is irrelevant as they are
   * evaluated "in parallel".
   */
  def splitPhis(): (List[PHI], List[Instruction]) = {
    var phis = Nil
    var rest = Instrs

    def split(l: List[Instruction]) : (List[PHI], List[Instruction]) = {
      l match {
        case PHI(a, xs) :: ls => {
          val (ys, zs) = split(ls)
          (PHI(a, xs) :: ys, zs)
        }
        case _ => (Nil, l)
      }
    }

    split(Instrs)
  }

  override def toString(): String = {
    var res = name + ":\n"
    for (i <- Instrs) {
      res += "  " + i + "\n"
    }
    res
  }
}

