package scanalyzer
package main

import analysis._
import analysis.constants._
import analysis.dominance._
import analysis.interpreter._
import analysis.signs._
import cfg._
import util._


object AnalysisOptions extends Enumeration {
  type AnalysisOptions = Value
  val NONE, INTERPRET, SIGN, CONST, SCCP, DOMINANCE = Value
}

import AnalysisOptions._

object Main extends App {
  var filename: String = null
  var do_printing = false
  var do_domtree = false
  var analysis_opt = NONE

  args foreach {
    case "-interpret" => analysis_opt = INTERPRET
    case "-sign" => analysis_opt = SIGN
    case "-const" => analysis_opt = CONST
    case "-sccp" => analysis_opt = SCCP
    case "-dom" => analysis_opt = DOMINANCE
    case "-domtree" => do_domtree = true
    case "-print" => do_printing = true
    case "-v" | "-verbose" => Util.dbglvl = 1
    case "-h" | "-help" => {
      println("scanalyzer - a simple abstract interpreter")
      println("Usage:")
      println("  scanalyzer [flags] <cfg file>")
      println("Supported analysis flags (use at most one):")
      println("  -interpret - interpret the cfg file")
      println("  -sign      - compute the possible signs of each variable")
      println("  -const     - check for constant values")
      println("  -sccp      - check for constant values and unreachable blocks")
      println("  -dom       - compute dominance information for the cfg")
      println("Supported additional flags:")
      println("  -print     - pretty print the input cfg")
      println("  -domtree   - print the dominance tree of the input cfg")
      println("  -verbose   - print additional verbose output")
      println("  -help      - display this help")
      System.exit(0)
    }
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

    val tree = DomTree.construct(fun)

    if (do_domtree) {
      println(tree.toString())
    }

    tree.verifySSA()

    var analysis: Analysis = null

    analysis_opt match {
      case INTERPRET => analysis = new Interpreter(fun)
      case SIGN => analysis = new SignAnalysis(fun)
      case CONST => analysis = new ConstantAnalysis(fun)
      case SCCP => analysis = new SCCPAnalysis(fun)
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

