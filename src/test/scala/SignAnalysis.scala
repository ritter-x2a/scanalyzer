import org.scalatest._

import scanalyzer.cfg._
import scanalyzer.analysis.signs._

class SignAnalysisSpec extends FlatSpec with Matchers {
  "SignAnalysis" should "compute a sound result for ex01.cfg" in {
    val fun = Parser.parse("examplefiles/ex01.cfg")
    val analysis = new SignAnalysis(fun)
    analysis.run
    val res = analysis.getResult
    res should include ("""phi1 -> GEZ()""")
    res should include ("""cmp1 -> GEZ()""")
    res should include ("""inc1 -> GZ()""")
  }
}
