package scanalyzer
package testing

import analysis.constants._
import cfg._

import org.scalatest._

class ConstAnalysisSpec extends FlatSpec with Matchers {
  "ConstantAnalysis" should "compute precise results for arithmetic.cfg" in {
    val fun = Parser.parse("examplefiles/arithmetic.cfg")
    val analysis = new ConstantAnalysis(fun)
    analysis.run
    val res = analysis.getResult
    res should include ("""x1 -> VAL(1)""")
    res should include ("""x2 -> VAL(3)""")
    res should include ("""x3 -> VAL(5)""")
    res should include ("""x4 -> VAL(8)""")

    res should include ("""y1 -> VAL(-1)""")
    res should include ("""y2 -> VAL(-3)""")
    res should include ("""y3 -> VAL(5)""")
    res should include ("""y4 -> VAL(-8)""")

    res should include ("""z1 -> VAL(-1)""")
    res should include ("""z2 -> VAL(-2)""")
    res should include ("""z3 -> VAL(-4)""")
    res should include ("""z4 -> VAL(8)""")

    res should include ("""w1 -> VAL(43)""")
    res should include ("""w2 -> VAL(21)""")
    res should include ("""w3 -> VAL(0)""")
    res should include ("""w4 -> VAL(0)""")
  }

  it should "compute precise results constant PHIs" in {
    val fun = Parser.parse("examplefiles/phi_eval.cfg")
    val analysis = new ConstantAnalysis(fun)
    analysis.run
    val res = analysis.getResult
    res should include ("""phi2 -> VAL(42)""")
  }
}

class SCCPAnalysisSpec extends FlatSpec with Matchers {
  "SCCPAnalysis" should "compute precise results for arithmetic.cfg" in {
    val fun = Parser.parse("examplefiles/arithmetic.cfg")
    val analysis = new SCCPAnalysis(fun)
    analysis.run
    val res = analysis.getResult
    res should include ("""x1 -> VAL(1)""")
    res should include ("""x2 -> VAL(3)""")
    res should include ("""x3 -> VAL(5)""")
    res should include ("""x4 -> VAL(8)""")

    res should include ("""y1 -> VAL(-1)""")
    res should include ("""y2 -> VAL(-3)""")
    res should include ("""y3 -> VAL(5)""")
    res should include ("""y4 -> VAL(-8)""")

    res should include ("""z1 -> VAL(-1)""")
    res should include ("""z2 -> VAL(-2)""")
    res should include ("""z3 -> VAL(-4)""")
    res should include ("""z4 -> VAL(8)""")

    res should include ("""w1 -> VAL(43)""")
    res should include ("""w2 -> VAL(21)""")
    res should include ("""w3 -> VAL(0)""")
    res should include ("""w4 -> VAL(0)""")

    res should include ("""start -> TOP()""")
  }

  it should "compute precise results constant PHIs" in {
    val fun = Parser.parse("examplefiles/phi_eval.cfg")
    val analysis = new SCCPAnalysis(fun)
    analysis.run
    val res = analysis.getResult
    res should include ("""phi2 -> VAL(42)""")
  }

  it should "handle obviously unreachable Blocks correctly" in {
    val fun = Parser.parse("examplefiles/sccp01.cfg")
    val analysis = new SCCPAnalysis(fun)
    analysis.run
    val res = analysis.getResult
    res should include ("""phi1 -> VAL(0)""")
    res should include ("""cmp1 -> VAL(1)""")
    res should include ("""BB_A -> TOP()""")
    res should include ("""BB_B -> UNREACH()""")
    res should include ("""BB_C -> TOP()""")
  }
}

