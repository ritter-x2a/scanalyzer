package scanalyzer.analysis

import scanalyzer.util._

case class AnalysisException(msg:String) extends ScanalyzerException(msg)

/**
 * General interface for analyses that work on function CFGs.
 */
trait Analysis {
  def run(): Unit
  def getResult(): String
}

