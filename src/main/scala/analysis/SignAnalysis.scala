package analysis

import cfg._

import scala.collection.mutable.{Set, Queue, Map}

trait AbstractVal[A] {
  def join(other: A): A

  def add(other: A): A
  def sub(other: A): A
  def mul(other: A): A
  def div(other: A): A

  def slt(other: A): A

  def canBeZero(): Boolean
  def canBeNonZero(): Boolean
}

sealed abstract class SignVal extends AbstractVal[SignVal] {
  override def join(other: SignVal): SignVal = {
    (this, other) match {
      case (TOP(), _) | (_, TOP()) => TOP()
      case (BOT(), o) => o
      case (t, BOT()) => t
      case (t, o) if t == o => t

      case (EZ(), GZ()) => GEZ()
      case (EZ(), LZ()) => LEZ()
      case (EZ(), LEZ()) => LEZ()
      case (EZ(), GEZ()) => GEZ()
      case (EZ(), NEZ()) => TOP()

      case (GZ(), LZ()) => NEZ()
      case (GZ(), LEZ()) => TOP()
      case (GZ(), GEZ()) => GEZ()
      case (GZ(), NEZ()) => NEZ()

      case (LZ(), LEZ()) => LEZ()
      case (LZ(), GEZ()) => TOP()
      case (LZ(), NEZ()) => NEZ()

      case (LEZ(), GEZ()) => TOP()
      case (LEZ(), NEZ()) => TOP()

      case (GEZ(), NEZ()) => TOP()

      case (t, o) => o join t
    }
  }

  def add(other: SignVal): SignVal = {
    (this, other) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()
      case (TOP(), _) | (_, TOP()) => TOP()
      case (NEZ(), NEZ()) => TOP()
      case (t, o) if t == o => t

      case (EZ(), GZ()) => GZ()
      case (EZ(), LZ()) => LZ()
      case (EZ(), LEZ()) => LEZ()
      case (EZ(), GEZ()) => GEZ()
      case (EZ(), NEZ()) => NEZ()

      case (GZ(), LZ()) => TOP()
      case (GZ(), LEZ()) => TOP()
      case (GZ(), GEZ()) => GZ()
      case (GZ(), NEZ()) => TOP()

      case (LZ(), LEZ()) => LZ()
      case (LZ(), GEZ()) => TOP()
      case (LZ(), NEZ()) => TOP()

      case (LEZ(), GEZ()) => TOP()
      case (LEZ(), NEZ()) => TOP()

      case (GEZ(), NEZ()) => TOP()

      case (t, o) => o add t
    }
  }

  private def invert(): SignVal = {
    this match {
      case LZ() => GZ()
      case GZ() => LZ()
      case LEZ() => GEZ()
      case GEZ() => LEZ()
      case s => s
    }
  }

  def sub(other: SignVal): SignVal = this add (other.invert())

  def mul(other: SignVal): SignVal = {
    (this, other) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()
      case (EZ(), _) => EZ()
      case (_, EZ()) => EZ()
      case (TOP(), _) | (_, TOP()) => TOP()

      case (NEZ(), NEZ()) => NEZ()
      case (GEZ(), GEZ()) => GEZ()
      case (LEZ(), LEZ()) => GEZ()
      case (GZ(), GZ()) => GZ()
      case (LZ(), LZ()) => GZ()

      case (GZ(), LZ()) => LZ()
      case (GZ(), LEZ()) => LEZ()
      case (GZ(), GEZ()) => GEZ()
      case (GZ(), NEZ()) => NEZ()

      case (LZ(), LEZ()) => GEZ()
      case (LZ(), GEZ()) => LEZ()
      case (LZ(), NEZ()) => NEZ()

      case (LEZ(), GEZ()) => LEZ()
      case (LEZ(), NEZ()) => TOP()

      case (GEZ(), NEZ()) => TOP()

      case (t, o) => o mul t
    }
  }

  def div(other: SignVal): SignVal = {
    (this, other) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()
      case (_, EZ()) => BOT()
      case (EZ(), _) => EZ()
      case (TOP(), _) | (_, TOP()) => TOP()

      case (NEZ(), _) | (_, NEZ()) => TOP()

      case (GEZ(), GEZ()) => GEZ()
      case (LEZ(), LEZ()) => GEZ()
      case (GZ(), GZ()) => GEZ() // Integer division!
      case (LZ(), LZ()) => GEZ()

      case (GZ(), LZ()) => LEZ()
      case (GZ(), LEZ()) => LEZ()
      case (GZ(), GEZ()) => GEZ()

      case (LZ(), GZ()) => LEZ()
      case (LZ(), LEZ()) => GEZ()
      case (LZ(), GEZ()) => LEZ()

      case (LEZ(), GZ()) => LEZ()
      case (LEZ(), LZ()) => GEZ()
      case (LEZ(), GEZ()) => LEZ()

      case (GEZ(), GZ()) => GEZ()
      case (GEZ(), LZ()) => LEZ()
      case (GEZ(), LEZ()) => LEZ()
    }
  }

  def slt(other: SignVal): SignVal =
    if ((this sub other) == LZ()) GZ() else EZ()

  override def canBeZero(): Boolean = (this join NEZ()) == TOP()

  override def canBeNonZero(): Boolean = (this join EZ()) != EZ()
}

case class BOT() extends SignVal
case class EZ() extends SignVal
case class GZ() extends SignVal
case class LZ() extends SignVal
case class LEZ() extends SignVal
case class GEZ() extends SignVal
case class NEZ() extends SignVal
case class TOP() extends SignVal

/**
 * A simple sign analysis that determines the possible signs of all occuring
 * values in the function.
 */
class SignAnalysis(fun: Function) extends ValueAnalysis[SignVal](fun) {
  private def err(msg: String) = throw new AnalysisException(msg)

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
        symtab(n) = l.foldLeft(BOT(): SignVal) ((a, i) => a join getVal(i._1))
      }
      case _ => err("Invalid named Instruction: `" + i + "`!")
    }
  }

  override def run(): Unit = {
    populateSymbolTable(new BOT())

    /** mapping (instr -> instrs directly depending on it) */
    val depMap = Map[Named, Set[Named]]()

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

    val queue = new Queue[Named]

    queue ++= depMap.keys

    while (! queue.isEmpty) {
      val current = queue.dequeue
      val before = symtab(current.Name)
      eval(current)
      val after = symtab(current.Name)
      if (after != before) {
        queue ++= (depMap(current) filterNot (queue contains _))
      }
    }
  }

  override def fromBigInt(x: BigInt): SignVal = {
    if (x > 0) {
      GZ()
    } else if (x < 0) {
      LZ()
    } else {
      EZ()
    }
  }
}
