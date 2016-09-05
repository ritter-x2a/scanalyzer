package scanalyzer
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

  def strIt(s: Iterable[Instruction]): String = {
    s.foldLeft("") ({
      case (a: String, i: Named) => a + " " + i.Name
      case (a: String, i: B) => a + " [some branch]"
      case (a: String, _) => a
    })
  }

  def strItBB(s: Iterable[BasicBlock]): String = {
    s.foldLeft("") ({(a: String, i: BasicBlock) => a + " " + i.Name})
  }

  def dbg(): Boolean = dbglvl > 0

}
