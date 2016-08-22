package analysis

import cfg._

trait Analysis {
  def run(): Unit
  def printResult(): Unit
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
