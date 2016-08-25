package main

import cfg._
import analysis._


object AnalysisOptions extends Enumeration {
  type AnalysisOptions = Value
  val NONE, INTERPRET = Value
}

import AnalysisOptions._

object Main extends App {
  var filename: String = null
  var do_printing = false
  var analysis_opt = NONE

  args foreach {
    case "-interpret" => analysis_opt = INTERPRET
    case "-print" => do_printing = true
    case s => filename = s
  }

  if (filename == null) {
    System.err.println("Input file required!")
    System.exit(1)
  }

  val fun = Parser.parse(filename)

  if (do_printing) {
    println(fun.toString)
  }

  var analysis: Analysis = null

  analysis_opt match {
    case INTERPRET => analysis = new Interpreter(fun)
    case _ =>
  }

  if (analysis == null) {
    System.exit(0)
  }

  analysis.run
  println(analysis.getResult)
}

