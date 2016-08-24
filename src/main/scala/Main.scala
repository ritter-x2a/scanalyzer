package main

import cfg._
import analysis._


object Main extends App {
  // val fun = new Function("foo")
  //
  // val bb_start = new BasicBlock("start")
  // val bb_A = new BasicBlock("BB_A")
  // val bb_B = new BasicBlock("BB_B")
  // val bb_C = new BasicBlock("BB_C")
  //
  // fun.First = bb_start
  //
  // bb_start.Instrs = B(Const(1), bb_A, bb_A) :: Nil
  //
  // var phi1 = PHI("phi1", (Const(0), bb_start) :: (Undef("inc1"), bb_A) :: Nil)
  // val cmp1 = SLT("cmp1", phi1, Const(10))
  //
  // val add1 = ADD("inc1", phi1, Const(1))
  // phi1.Ops = phi1.Ops.head :: (add1, bb_B) :: Nil
  //
  // bb_A.Instrs = phi1 :: cmp1 :: B(cmp1, bb_B, bb_C) :: Nil
  // bb_B.Instrs = add1 :: B(Const(1), bb_A, bb_A) :: Nil
  // bb_C.Instrs = RET(phi1) :: Nil
  //
  //
  // println(bb_start)
  // println(bb_A)
  // println(bb_B)
  // println(bb_C)

  Parser.parse("examplefiles/ex01.cfg")

  // val interpreter = new Interpreter(fun)
  //
  // interpreter.run
  // interpreter.printResult
}

