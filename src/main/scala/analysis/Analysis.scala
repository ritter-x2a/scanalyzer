package Analysis

import cfg._

trait Analysis {
  def run()
  def printResult()
}

abstract class ValueAnalysis[T](fun: Function) extends Analysis {
  var symtab: Map[String, Option[T]] = Map()

  def populateSymbolTable() = {
    fun.traverseInstructions((i: Instruction) => {
      i match {
        case Named(n) if !(symtab contains n) =>
            symtab += (n -> None)
        case _ => None
      }
    })
  }
}


sealed abstract class SignVal
case class EZ()
case class GZ()
case class LZ()
case class LEZ()
case class GEZ()
case class NEZ()
case class TOP()

class SignAnalysis(fun: Function) extends ValueAnalysis[SignVal](fun) {
  override def run() = {

  }

  override def printResult() = {

  }
}


