package scanalyzer.analysis

import scanalyzer.util._
import scanalyzer.cfg._
import scala.collection.mutable.{Set, Queue, Map}

/**
 * Implementation of a simple worklist algorithm for value analyses.
 *
 * Reachable blocks (and control flow in general) are ignored and soundly
 * over-approximated.
 */
abstract class SimpleValueIterationAnalysis[A <: AbstractVal[A]](
    fun: Function)
  extends ValueAnalysis[A](fun) {

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
        symtab(n) = l.foldLeft(bot()) ((a, i) => a join getVal(i._1))
      }
      case _ => err("Invalid named Instruction: `" + i + "`!")
    }
  }

  /**
   * Set up the given map to contain an entry (i -> {j: j depends on i}).
   */
  private def setUpDependencyMap(depMap: Map[Named, Set[Named]]) = {
    // initialize depMap with empty sets
    fun.traverseInstructions {
      case i @ Named(_) => depMap += (i -> Set[Named]())
      case _ =>
    }

    val addIfNamed: (Named, Value) => Unit = {
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
      case _ =>
    }
  }

  override def run(): Unit = {
    populateSymbolTable(bot())

    /** mapping (instr -> instrs directly depending on it) */
    val depMap = Map[Named, Set[Named]]()

    setUpDependencyMap(depMap)

    Util.dbgmsg("Dependencies:")
    if (Util.dbg) {
      depMap foreach {
        case (Named(n), s) => Util.dbgmsg("  " + n + " ->" + Util.strIt(s))
        case _ =>
      }
    }

    val queue = new Queue[Named]

    queue ++= depMap.keys

    while (! queue.isEmpty) {
      Util.dbgmsg("Queue: " + Util.strIt(queue))

      val current = queue.dequeue
      val before = symtab(current.Name)
      eval(current)
      val after = symtab(current.Name)

      Util.dbgmsg("Transform `" + current.Name + "` from `" + before +
        "` to `" + after)

      if (after != before) {
        val add = (depMap(current) filterNot (queue contains _))
        Util.dbgmsg("  ~> Adding `" + Util.strIt(add) + "` to queue")
        queue ++= add
      }
    }
  }
}

