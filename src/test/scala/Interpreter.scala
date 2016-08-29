import org.scalatest._

import cfg._
import analysis._

class InterpreterSpec extends FlatSpec with Matchers {
  "Interpreter" should "compute the result 10" in {
    val fun = new Function("foo")

    val bb_start = new BasicBlock("start")
    val bb_A = new BasicBlock("BB_A")
    val bb_B = new BasicBlock("BB_B")
    val bb_C = new BasicBlock("BB_C")

    fun.first = bb_start

    bb_start.Instrs = B(Const(1), bb_A, bb_A) :: Nil

    var phi1 = PHI("phi1", (Const(0), bb_start) :: (Undef("inc1"), bb_A) :: Nil)
    val cmp1 = BinOp("cmp1", SLT(), phi1, Const(10))

    val add1 = BinOp("inc1", ADD(), phi1, Const(1))
    phi1.Ops = phi1.Ops.head :: (add1, bb_B) :: Nil

    bb_A.Instrs = phi1 :: cmp1 :: B(cmp1, bb_B, bb_C) :: Nil
    bb_B.Instrs = add1 :: B(Const(1), bb_A, bb_A) :: Nil
    bb_C.Instrs = RET(phi1) :: Nil

    val interpreter = new Interpreter(fun)

    interpreter.run
    interpreter.getResult should include ("""__RES__ -> Some(10)""")
  }

  it should "handle multiple PHIs correctly" in {
    val fun = Parser.parse("examplefiles/phi_eval.cfg")
    val interpreter = new Interpreter(fun)
    interpreter.run
    val res = interpreter.getResult
    res should include ("""__RES__ -> Some(10)""")
    res should include ("""phi2 -> Some(42)""")
  }

  it should "handle BigInt constants correctly" in {
    val fun = Parser.parse("examplefiles/bigint.cfg")
    val interpreter = new Interpreter(fun)
    interpreter.run
    val res = interpreter.getResult
    res should include ("""x -> Some(1000000000000000000)""")
    res should include ("""__RES__ -> Some(1000000000000000000)""")
  }

  it should "handle all arithmetic operators correctly" in {
    val fun = Parser.parse("examplefiles/arithmetic.cfg")
    val interpreter = new Interpreter(fun)
    interpreter.run
    val res = interpreter.getResult
    res should include ("""x1 -> Some(1)""")
    res should include ("""x2 -> Some(3)""")
    res should include ("""x3 -> Some(5)""")
    res should include ("""x4 -> Some(8)""")

    res should include ("""y1 -> Some(-1)""")
    res should include ("""y2 -> Some(-3)""")
    res should include ("""y3 -> Some(5)""")
    res should include ("""y4 -> Some(-8)""")

    res should include ("""z1 -> Some(-1)""")
    res should include ("""z2 -> Some(-2)""")
    res should include ("""z3 -> Some(-4)""")
    res should include ("""z4 -> Some(8)""")

    res should include ("""w1 -> Some(43)""")
    res should include ("""w2 -> Some(21)""")
    res should include ("""w3 -> Some(0)""")
    res should include ("""w4 -> Some(0)""")
  }
}
