package analysis

import cfg._

/**
 * An Interpreter for the SSA-CFG format that implicitly defines its semantics.
 */
class Interpreter(fun: Function) extends ValueAnalysis[BigInt](fun) {
  override def run(): Unit = {
    populateSymbolTable()
    var currBB = fun.First



  }

  override def printResult() = {

  }
}
