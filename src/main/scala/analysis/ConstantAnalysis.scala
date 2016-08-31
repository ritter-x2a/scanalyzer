package scanalyzer
package analysis
package constants

import cfg._

/**
 * AbstractVal implementation for the Sign domain, represents the scalar
 * relation of a value with zero.
 */
sealed abstract class ConstVal extends AbstractVal[ConstVal] {
  override def join(other: ConstVal): ConstVal = {
    (this, other) match {
      case (TOP(), _) | (_, TOP()) => TOP()
      case (BOT(), o) => o
      case (t, BOT()) => t
      case (t, o) if t == o => t
      case (VAL(_), VAL(_)) => TOP()
    }
  }

  private def constOp(
      fun: (BigInt, BigInt) => BigInt,
      t: ConstVal,
      o: ConstVal
    ): ConstVal = {
    (t, o) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()
      case (TOP(), _) | (_, TOP()) => TOP()
      case (VAL(x), VAL(y)) => VAL(fun(x, y))
    }
  }

  override def add(other: ConstVal): ConstVal =
    constOp((x, y) => x + y, this, other)

  override def sub(other: ConstVal): ConstVal =
    constOp((x, y) => x - y, this, other)

  override def mul(other: ConstVal): ConstVal =
    constOp((x, y) => x * y, this, other)

  override def div(other: ConstVal): ConstVal =
    if (other != VAL(0)) constOp((x, y) => x / y, this, other) else BOT()

  override def slt(other: ConstVal): ConstVal =
    constOp((x, y) => if (x < y) 1 else 0, this, other)

  override def canBeZero(): Boolean = {
    this match {
      case TOP() => true
      case VAL(x) => x != 0
      case _ => false
    }
  }

  override def canBeNonZero(): Boolean = (this join VAL(0)) != VAL(0)
}

case class BOT() extends ConstVal
case class VAL(x: BigInt) extends ConstVal
case class TOP() extends ConstVal

/**
 * A simple sign analysis that determines the possible signs of all occuring
 * values in the function.
 */
class ConstantAnalysis(fun: Function)
  extends SimpleValueIterationAnalysis[ConstVal](fun) {
  override def bot(): ConstVal = BOT()

  override def fromBigInt(x: BigInt): ConstVal = VAL(x)
}
