package analysis

case class AnalysisException(msg:String) extends Exception

/**
 * General interface for analyses that work on function CFGs.
 */
trait Analysis {
  def run(): Unit
  def getResult(): String
}

