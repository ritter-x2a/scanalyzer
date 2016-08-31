package scanalyzer.main

import scanalyzer.util._
import scanalyzer.cfg._
import scanalyzer.analysis._
import scanalyzer.analysis.signs._
import scanalyzer.analysis.constants._
import scanalyzer.analysis.dominance._


object AnalysisOptions extends Enumeration {
  type AnalysisOptions = Value
  val NONE, INTERPRET, SIGN, CONST, DOMINANCE = Value
}

import AnalysisOptions._

object Main extends App {
  var filename: String = null
  var do_printing = false
  var analysis_opt = NONE

  args foreach {
    case "-interpret" => analysis_opt = INTERPRET
    case "-sign" => analysis_opt = SIGN
    case "-const" => analysis_opt = CONST
    case "-dom" => analysis_opt = DOMINANCE
    case "-print" => do_printing = true
    case "-v" => Util.dbglvl = 1
    case s => filename = s
  }

  if (filename == null) {
    System.err.println("Input file required!")
    System.exit(2)
  }

  try {
    val fun = Parser.parse(filename)

    if (do_printing) {
      println(fun.toString)
    }

    var analysis: Analysis = null

    analysis_opt match {
      case INTERPRET => analysis = new Interpreter(fun)
      case SIGN => analysis = new SignAnalysis(fun)
      case CONST => analysis = new ConstantAnalysis(fun)
      case DOMINANCE => analysis = new DominanceAnalysis(fun)
      case _ =>
    }

    if (analysis == null) {
      System.exit(0)
    }

    analysis.run
    println(analysis.getResult)
  } catch {
    case ScanalyzerException(msg) => {
      System.err.println("Error: " + msg)
      System.exit(1)
    }
    case e: Exception => throw e
  }
}

