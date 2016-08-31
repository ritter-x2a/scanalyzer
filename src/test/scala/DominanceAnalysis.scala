import org.scalatest._

import scanalyzer.cfg._
import scanalyzer.analysis._
import scanalyzer.analysis.dominance._

class DominanceSpec extends FlatSpec with Matchers {

  "DominanceAnalysis" should "compute correct dominance information for ex01" in {
    val fun = Parser.parse("examplefiles/ex01.cfg")
    val analysis = new DominanceAnalysis(fun)
    analysis.run
    val res = analysis.getMapping()

    fun foreach (bb => {
      bb.Name match {
        case "start" => {
          res(bb).size should be (1)
          (res(bb) map (_.Name)) should contain ("start")
        }
        case "BB_A" => {
          res(bb).size should be (2)
          (res(bb) map (_.Name)) should contain ("start")
          (res(bb) map (_.Name)) should contain ("BB_A")
        }
        case "BB_B" => {
          res(bb).size should be (3)
          (res(bb) map (_.Name)) should contain ("start")
          (res(bb) map (_.Name)) should contain ("BB_A")
          (res(bb) map (_.Name)) should contain ("BB_B")
        }
        case "BB_C" => {
          res(bb).size should be (3)
          (res(bb) map (_.Name)) should contain ("start")
          (res(bb) map (_.Name)) should contain ("BB_A")
          (res(bb) map (_.Name)) should contain ("BB_C")
        }
        case _ =>
      }
    })
  }

  it should "compute correct dominance information for dom_complex" in {
    val fun = Parser.parse("examplefiles/dom_complex.cfg")
    val analysis = new DominanceAnalysis(fun)
    analysis.run
    val res = analysis.getMapping()

    fun foreach (bb => {
      bb.Name match {
        case "BB_1" => {
          res(bb).size should be (1)
          (res(bb) map (_.Name)) should contain ("BB_1")
        }
        case "BB_2" => {
          res(bb).size should be (2)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
        }
        case "BB_3" => {
          res(bb).size should be (3)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_3")
        }
        case "BB_4" => {
          res(bb).size should be (4)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_3")
          (res(bb) map (_.Name)) should contain ("BB_4")
        }
        case "BB_5" => {
          res(bb).size should be (5)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_3")
          (res(bb) map (_.Name)) should contain ("BB_4")
          (res(bb) map (_.Name)) should contain ("BB_5")
        }
        case "BB_6" => {
          res(bb).size should be (3)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_6")
        }
        case "BB_7" => {
          res(bb).size should be (4)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_6")
          (res(bb) map (_.Name)) should contain ("BB_7")
        }
        case "BB_8" => {
          res(bb).size should be (3)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_8")
        }
        case "BB_9" => {
          res(bb).size should be (4)
          (res(bb) map (_.Name)) should contain ("BB_1")
          (res(bb) map (_.Name)) should contain ("BB_2")
          (res(bb) map (_.Name)) should contain ("BB_8")
          (res(bb) map (_.Name)) should contain ("BB_9")
        }
        case _ =>
      }
    })
  }
}
