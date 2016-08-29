import cfg._
import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  def performParseTest(filename: String) = {
    val fun = Parser.parse(filename)
    val reference = scala.io.Source.fromFile(filename).mkString
    fun.toString should be (reference)
  }

  "Parser" should "parse the input CFG" in {
    performParseTest("examplefiles/ex01.cfg")
  }

  it should "throw ParserException if an undefined name is used" in {
    a [ParserException] should be thrownBy {
      performParseTest("examplefiles/neg01.cfg")
    }
  }
}
