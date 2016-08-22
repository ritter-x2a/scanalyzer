package analysis

import cfg._

sealed abstract class SignVal
case class EZ()
case class GZ()
case class LZ()
case class LEZ()
case class GEZ()
case class NEZ()
case class TOP()

/**
 * A simple sign analysis that determines the possible signs of all occuring
 * values in the function.
 */
class SignAnalysis(fun: Function) extends ValueAnalysis[SignVal](fun) {
  override def run() = {

  }

  override def printResult() = {

  }
}
