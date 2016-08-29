package util

import cfg._

/**
 * Superclass for custom Exceptions used in this project.
 */
class ScanalyzerException(msg: String) extends Exception(msg) {
  val Msg: String = msg
}

object ScanalyzerException extends Exception {
  def unapply(x: ScanalyzerException): Option[String] =
    Some(x.Msg)
}

object Util {
  var dbglvl = 0

  def dbgmsg(msg: String): Unit = {
    if (dbglvl > 0) {
      println(msg)
    }
  }

  def strIt(s: Iterable[Named]): String = {
    s.foldLeft("") ({(a: String, i: Named) => a + " " + i.Name})
  }

  def dbg(): Boolean = dbglvl > 0

}
