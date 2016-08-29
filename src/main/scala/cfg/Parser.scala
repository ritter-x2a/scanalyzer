package cfg

import scala.io.Source
import util._

case class ParserException(msg:String) extends ScanalyzerException(msg)

/**
 * States of the "control automaton" (in some way...) of the parser.
 */
object ParseState extends Enumeration {
  type ParseState = Value
  val NeedFunDef, // initial, expecting the start of a fundef
      NeedBB,     // after the start of a fundef, expects BB
      MayBB,      // after >= 1 full BB, end of fundef or new BB may follow
      MayPhi,     // after BB start and * PHI instrs, PHI or non-PHI may follow
      Instr,      // after a non-PHI, non-term instr, non-PHI instr may follow
      Done        // after a full fundef
      = Value
}

import ParseState._

object ParseRegEx {
  /** Pattern for valid identifiers. */
  val name_pat = "(?:[a-zA-Z][_a-zA-Z0-9]*)"

  /** Pattern for valid number constants. */
  val num_pat = "(?:\\-?(?:[0-9]|(?:[1-9][0-9]*)))"

  /** Pattern for valid values (i.e. identifiers or numbers). */
  val val_pat = "(?:" + name_pat + "|" + num_pat + ")"

  /** Pattern for the beginning of a function definition. */
  val fun_def_start_pat = ("\\s*fun\\s+(" + name_pat + ")\\s*\\{\\s*").r

  /** Pattern for the end of a function definition. */
  val fun_def_end_pat = "\\s*\\}(\\s*)".r

  /** Pattern for the beginning of a BasicBlock. */
  val bb_start_pat = ("\\s*(" + name_pat + ")\\s*:\\s*").r

  /** Pattern for an empty line. */
  val empty_pat = "(\\s*)".r

  /** Pattern for a binary operator instruction. */
  val binop_pat = (
    "\\s*(" + name_pat
    + ")\\s*=\\s*(" + name_pat
    + ")\\s*(" + val_pat
    + ")\\s*,\\s*(" + val_pat
    + ")\\s*").r

  /** Pattern for a RET instruction. */
  val ret_pat = ("\\s*RET\\s*(" + val_pat + ")\\s*").r

  /** Pattern for a B instruction. */
  val b_pat = ("\\s*B\\s*(" + val_pat + ")\\s*,\\s*(" + name_pat + ")\\s*,\\s*("
     + name_pat + ")\\s*").r

  /**
   * Pattern for a PHI instruction. As Scala regex matching is not expressive
   *  enough to get all operands nicely separated, some more processing is
   *  required here.
   */
  val phi_pat =
    ("\\s*(" + name_pat
      + ")\\s*=\\s*PHI((?:\\s*\\[\\s*" + val_pat
      + "\\s*,\\s*" + name_pat
      + "\\s*\\]\\s*,?)+)\\s*").r
}

import ParseRegEx._

/**
 * Encapsulates functions for parsing CFG input files.
 *
 * Makes use of obscure regular expressions. This is clearly bad for performance
 * but acceptable as performance is not in the primary focus of the project.
 */
object Parser {
  private def err(msg: String) = throw new ParserException(msg)

  /**
   * Creates either an integer constant value if the given string is a number or
   * otherwise an Undef Value with the given string as name
   */
  def makeDummyVal(s: String): Value = {
    val pat = ("(" + num_pat + ")").r
    s match {
      case pat(s) => Const(s.toInt)
      case s => Undef(s)
    }
  }

  private def parsePhiTail(tail: String): List[(Value, BasicBlock)] = {
    val phiarg_pat = ("(" + val_pat + ")\\s*,\\s*(" + name_pat + ")").r
    var ops: List[(Value, BasicBlock)] = Nil
    for (s <- tail.split("]").map(_.replaceAll("\\s*,?\\s*\\[", "")).
        map(_.trim)) {
      s match {
        case phiarg_pat(v, b) => {
          ops = (makeDummyVal(v), new BasicBlock(b)) :: ops
        }
        case _ => err("Invalid PHI argument: `" + s + "`!")
      }
    }
    ops.reverse
  }

  /**
   * Parses a Function from a CFG file with the given filename.
   *
   * Parsing works in two steps:
   *   - First, all BasicBlocks are created and filled with Instructions which
   *     only use dummy Values (and BasicBlocks).
   *   - Second, those dummy Values (and BasicBlocks) are replace by the real
   *     ones.
   *
   *  SSA properties are not fully checked and should be checked afterwords by
   *  calling the verify() methond of the result.
   */
  def parse(filename: String): Function = {
    var state = NeedFunDef
    var res: Function = null
    var currBB: BasicBlock = null
    var currInstrs: List[Instruction] = Nil
    val symtab = collection.mutable.Map[String, Named]()
    val bbtab = collection.mutable.Map[String, BasicBlock]()
    def closeBB() = {
      currBB.Instrs = currInstrs.reverse
      bbtab += (currBB.Name -> currBB)
      currBB = null
      currInstrs = Nil
    }
    def symtabLookUp(x: String): Named =
      symtab getOrElse (x, err("Use of undefined symbol `" + x + "`!"))


    // Step 1: construct CFG with dummy values
    for (line <- Source.fromFile(filename).getLines()) {
      line match {
        case fun_def_start_pat(name) if (state == NeedFunDef) => {
          res = new Function(name)
          state = NeedBB
        }
        case fun_def_end_pat(x) if (state == MayBB) => {
          state = Done
        }
        case bb_start_pat(name) if (state == NeedBB || state == MayBB) => {
          currBB = new BasicBlock(name)
          if (state == NeedBB) {
            res.first = currBB
          }
          state = MayPhi
        }
        case binop_pat(name,op,a,b) if (state == Instr || state == MayPhi) => {
          val operator = op match {
            case "ADD" => ADD()
            case "SUB" => SUB()
            case "MUL" => MUL()
            case "DIV" => DIV()
            case "SLT" => SLT()
            case _ => err("Invalid binary operator: `" + op + "`!")
          }
          val instr = BinOp(name, operator, makeDummyVal(a), makeDummyVal(b))
          symtab += (name -> instr)
          currInstrs = instr :: currInstrs
          state = Instr
        }
        case phi_pat(name, tail) if (state == MayPhi) => {
          val instr = PHI(name, parsePhiTail(tail))
          symtab += (name -> instr)
          currInstrs = instr :: currInstrs
          state = MayPhi
        }
        case ret_pat(v) if (state == Instr || state == MayPhi) => {
          currInstrs = RET(makeDummyVal(v)) :: currInstrs
          closeBB()
          state = MayBB
        }
        case b_pat(c,t,f) if (state == Instr || state == MayPhi) => {
          val instr = B(makeDummyVal(c), new BasicBlock(t), new BasicBlock(f))
          currInstrs = instr :: currInstrs
          closeBB()
          state = MayBB
        }
        case empty_pat(x) => ;
        case x => err("Invalid input line: `" + x + "`!")
      }
    }
    if (state != Done) {
      err("Unterminated input!")
    }

    // Step 2: fill in non-dummy values
    res.traverseInstructions((i: Instruction) => {
      i match {
        case s: BinOp => {
          s.OpA match {
            case Undef(n) => s.OpA = symtabLookUp(n)
            case _ => ;
          }
          s.OpB match {
            case Undef(n) => s.OpB = symtabLookUp(n)
            case _ => ;
          }
        }
        case s: PHI => {
          val newOps = s.Ops.map(pair =>
            (pair._1 match {
              case Undef(n) => symtabLookUp(n)
              case _ => pair._1
            }, bbtab(pair._2.Name))
          )
          s.Ops = newOps
        }
        case s: B => {
          s.C match {
            case Undef(n) => s.C = symtabLookUp(n)
            case _ => ;
          }
          s.TSucc = bbtab(s.TSucc.Name)
          s.FSucc = bbtab(s.FSucc.Name)
        }
        case s: RET => {
          s.Op match {
            case Undef(n) => s.Op = symtabLookUp(n)
            case _ => ;
          }
        }
        case _ => err("Unsupported Instruction!")
      }
    })

    res
  }
}
