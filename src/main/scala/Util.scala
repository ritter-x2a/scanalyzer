package util

import cfg._

object Util {
  var dbglvl = 0

  def dbgmsg(msg: String) = {
    if (dbglvl > 0) {
      println(msg)
    }
  }

  def strIt(s: Iterable[Named]): String = {
    s.foldLeft("") ({(a: String, i: Named) => a + " " + i.Name})
  }

  def dbg(): Boolean = dbglvl > 0

}
