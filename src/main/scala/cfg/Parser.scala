package cfg

import scala.io.Source

case class ParserException(msg:String) extends Exception

object ParseState extends Enumeration {
  type ParseState = Value
  val NeedFunDef, NeedBB, MayBB, MayPhi, Instr, Done = Value
}

import ParseState._

object Parser {
  val name_pat = "[a-zA-Z][_a-zA-Z0-9]*"
  val num_pat = "\\-?(?:[0-9]|[1-9][0-9]*)"
  val val_pat = "(?:"+name_pat+"|"+num_pat+")"

  val fun_decl_start_pat = ("\\s*fun\\s+("+name_pat+")\\s*\\{\\s*").r
  val fun_decl_end_pat = "\\s*\\}(\\s*)".r
  val bb_start_pat = ("\\s*("+name_pat+")\\s*:\\s*").r
  val empty_pat = "(\\s*)".r

  val binop_pat = (
    "\\s*("+name_pat
    +")\\s*=\\s*("+name_pat
    +")\\s*("+val_pat
    +")\\s*,\\s*("+val_pat
    +")\\s*").r

  val ret_pat = ("\\s*RET\\s*("+val_pat+")\\s*").r
  val b_pat = ("\\s*B\\s*("+val_pat+")\\s*,\\s*("+name_pat+")\\s*,\\s*("
    +name_pat+")\\s*").r
  val phi_pat =
    ("\\s*("+name_pat+")\\s*=\\s*PHI((?:\\s*\\[\\s*"+val_pat+"\\s*,\\s*"+name_pat+"\\s*\\]\\s*,?)+)\\s*").r

  def parse(filename: String): Function = {
    var state = NeedFunDef
    var res: Function = null
    var currBB: BasicBlock = null
    var currInstrs: List[Instruction] = Nil

    for (line <- Source.fromFile(filename).getLines()) {
      line match {
        case fun_decl_start_pat(name) if (state == NeedFunDef) => {
          res = new Function(name)
          state = NeedBB
        }
        case fun_decl_end_pat(x) if (state == MayBB) => {
          // TODO close BB
          state = Done
        }
        case bb_start_pat(name) if (state == NeedBB || state == MayBB) => {
          // TODO close BB
          if (state == NeedBB)
            res.First == currBB
          // TODO open BB
          state = MayPhi
        }
        case binop_pat(name,op,a,b) if (state == Instr || state == MayPhi) => {
          println("binop:"+name+op+a+b)
          state = Instr
        }
        case phi_pat(name, tail) if (state == MayPhi) => {
          println("phi:"+name+tail)
          state = MayPhi
        }
        case ret_pat(v) => {
          println("ret:"+v)
          state = MayBB
        }
        case b_pat(c,t,f) => {
          println("b:"+c+t+f)
          state = MayBB
        }
        case empty_pat(x) => ;
        case x => throw new ParserException("Invalid input line: `"+x+"`!")
      }
    }
    if (state != Done)
      throw new ParserException("Unterminated input!")
    res
  }
}
