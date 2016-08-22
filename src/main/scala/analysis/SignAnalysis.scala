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

class SignAnalysis(fun: Function) extends ValueAnalysis[SignVal](fun) {
  override def run() = {

  }

  override def printResult() = {

  }
}
