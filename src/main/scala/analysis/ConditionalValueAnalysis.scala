package scanalyzer
package analysis

import cfg._
import util._

import scala.collection.mutable.{Set, Queue, Map}


sealed abstract class ReachVal

case class UNREACH() extends ReachVal
case class TOP() extends ReachVal


/**
 * Implementation of a simple worklist algorithm for value analyses.
 *
 * Reachable blocks (and control flow in general) are ignored and soundly
 * over-approximated.
 */
abstract class ConditionalValueAnalysis[A <: AbstractVal[A]](
    fun: Function)
  extends ValueAnalysis[A](fun)
  with BlockAnalysis[ReachVal] {

  private def err(msg: String) = throw new AnalysisException(msg)

  /**
   * Returns the unique Bottom element of the used abstract domain.
   *
   * This has to happen here because scala does not support abstract static
   * methods.
   */
  protected def bot(): A

  /**
   * Applies the effect of the given named instruction to the symbol table.
   */
  private def eval(i: Named): Unit = {
    i match {
      case BinOp(n, op, a, b) => {
        val aval = getVal(a)
        val bval = getVal(b)
        symtab(n) = op match {
          case ADD() => aval add bval
          case SUB() => aval sub bval
          case MUL() => aval mul bval
          case DIV() => aval div bval
          case SLT() => aval slt bval
        }
      }
      case PHI(n, l) => {
        symtab(n) = l.foldLeft(bot()) {
          case (a, (v, bb)) =>  {
            blocktab(bb) match {
              case UNREACH() => a
              case TOP() => a join getVal(v)
            }
          }
        }
      }
      case _ => err("Invalid named Instruction: `" + i + "`!")
    }
  }

  /**
   * Set up the given map to contain an entry (i -> {j: j depends on i}).
   */
  private def setUpDependencyMap(depMap: Map[Named, Set[Instruction]]) = {
    // initialize depMap with empty sets
    fun.traverseInstructions {
      case i @ Named(_) => depMap += (i -> Set[Instruction]())
      case _ =>
    }

    val addIfNamed: (Instruction, Value) => Unit = {
      case (i, x: Named) => depMap(x) += i
      case _ =>
    }

    // add instructions that directly depend
    fun.traverseInstructions {
      case i @ BinOp(_, _, a, b) => {
        addIfNamed(i, a)
        addIfNamed(i, b)
      }
      case i @ PHI(_, l) => {
        l foreach {
          case (v, bb) => addIfNamed(i, v)
          case _ =>
        }
      }
      case i @ B(c, _, _) => {
        addIfNamed(i, c)
      }
      case _ =>
    }
  }

  sealed abstract class QueueItem
  case class INSTR(i: Instruction) extends QueueItem
  case class BB(i: BasicBlock) extends QueueItem

  override def run(): Unit = {
    populateSymbolTable(bot())

    populateBlockTable(fun, () => UNREACH())
    blocktab(fun.first) = TOP()

    /** mapping (instr -> instrs directly depending on it) */
    val depMap = Map[Named, Set[Instruction]]()

    setUpDependencyMap(depMap)

    Util.dbgmsg("Dependencies:")
    if (Util.dbg) {
      depMap foreach {
        case (Named(n), s) =>
          Util.dbgmsg("  " + n + " ->" + Util.strIt(s))
        case _ =>
      }
    }

    val queue = new Queue[QueueItem]

    def addInstrsToQueue(xs: Iterable[Instruction]) = {
      val addInstrs = (xs filterNot (queue contains _))
      Util.dbgmsg("  ~> Adding instructions `" + Util.strIt(addInstrs) + "` to queue")
      val add = addInstrs map (INSTR(_))
      queue ++= add
    }

    def addBBToQueue(bb: BasicBlock) = {
      if (!(queue contains BB(bb))) {
        Util.dbgmsg("  ~> Adding BasicBlock `" + bb.Name + "` to queue")
        queue += BB(bb)
      }
    }

    def canBeNonZero(v: Value) = {
      v match {
        case Named(n) => symtab(n).canBeNonZero()
        case Const(x) => x != 0
        case Undef(_) => true
      }
    }

    def canBeZero(v: Value) = {
      v match {
        case Named(n) => symtab(n).canBeZero()
        case Const(x) => x == 0
        case Undef(_) => true
      }
    }

    val terminators = fun map (_.getTerminator())

    queue ++= (depMap.keys map (INSTR(_)))
    queue ++= (terminators map (INSTR(_)))

    while (! queue.isEmpty) {
      if (Util.dbg) {
        val s = queue.foldLeft("") ({
          case (a: String, INSTR(i: Named)) => a + " " + i.Name
          case (a: String, INSTR(_: B)) => a + " [some branch]"
          case (a: String, BB(b)) => a + " " + b.Name
          case (a: String, _) => a
        })
        Util.dbgmsg("Queue: " + s)
      }

      queue.dequeue match {
        case INSTR(instr: Named) => {
          val before = symtab(instr.Name)
          eval(instr)
          val after = symtab(instr.Name)

          Util.dbgmsg("Transform `" + instr.Name + "` from `" + before +
            "` to `" + after + "`")

          if (after != before) {
            addInstrsToQueue(depMap(instr))
          }
        }
        case INSTR(instr @ B(c, a, b)) if blocktab(instr.parent) == TOP() => {
          if (canBeNonZero(c) && !(blocktab(a) == TOP())) {
            addBBToQueue(a)
          }
          if (canBeZero(c) && !(blocktab(b) == TOP())) {
            addBBToQueue(b)
          }
        }
        case BB(bb) => {
          blocktab(bb) = TOP()
          Util.dbgmsg("Transform `" + bb.Name + "` to `TOP`")
          val terminator = bb.getTerminator()
          addInstrsToQueue(List(terminator))
          terminator match {
            case B(_, a, b) => {
              addInstrsToQueue(a.splitPhis()._1)
              addInstrsToQueue(b.splitPhis()._1)
            }
            case _ =>
          }
        }
        case _ =>
      }
    }
  }

  override def getResult(): String = {
    var res = ""
    for ((k, v) <- symtab)
      res += k + " -> " + v + "\n"
    for ((k, v) <- blocktab)
      res += k.Name + " -> " + v + "\n"
    res
  }
}

