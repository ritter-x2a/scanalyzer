package scanalyzer
package testing

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

  it should "parse BigInt constants correctly" in {
    performParseTest("examplefiles/bigint.cfg")
  }

  it should "parse arithmetic operations correctly" in {
    performParseTest("examplefiles/arithmetic.cfg")
  }

  it should "parse divisions by zero correctly" in {
    performParseTest("examplefiles/neg_divzero.cfg")
  }

  it should "ignore comments" in {
    val fun = Parser.parse("examplefiles/comment.cfg")
    val reference = scala.io.Source.fromFile("examplefiles/ex01.cfg").mkString
    fun.toString should be (reference)
  }

  it should "throw ParserException if a value name is used twice" in {
    a [ParserException] should be thrownBy {
      performParseTest("examplefiles/neg02.cfg")
    }
  }

  it should "throw ParserException if a BB name is used twice" in {
    a [ParserException] should be thrownBy {
      performParseTest("examplefiles/neg03.cfg")
    }
  }

  it should "throw ParserException if an undefined name is used" in {
    a [ParserException] should be thrownBy {
      performParseTest("examplefiles/neg01.cfg")
    }
  }
}
