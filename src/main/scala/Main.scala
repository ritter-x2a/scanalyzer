package main

import cfg._
import analysis._


object Main extends App {
  val fun = Parser.parse("examplefiles/ex01.cfg")

  println(""+fun)

  val interpreter = new Interpreter(fun)

  interpreter.run
  println(interpreter.getResult)
}

