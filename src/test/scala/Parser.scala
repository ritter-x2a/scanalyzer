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

  // it should "throw NoSuchElementException if an empty stack is popped" in {
  //   val emptyStack = new Stack[Int]
  //   a [NoSuchElementException] should be thrownBy {
  //     emptyStack.pop()
  //   }
  // }
}
