package scanalyzer
package analysis
package intervals

import cfg._

/**
 * AbstractVal implementation for the Intervals domain.
 */
sealed abstract class InterVal extends AbstractVal[InterVal] {
  override def join(other: InterVal): InterVal = {
    (this, other) match {
      case (TOP(), _) | (_, TOP()) => TOP()
      case (BOT(), o) => o
      case (t, BOT()) => t
      case (LEQ(_), GEQ(_)) => TOP()
      case (LEQ(ha), LEQ(hb)) => LEQ(max(ha, hb))
      case (GEQ(la), GEQ(lb)) => GEQ(min(la, lb))
      case (GEQ(la), INT(lb, _)) => GEQ(min(la, lb))
      case (LEQ(ha), INT(_, hb)) => LEQ(max(ha, hb))
      case (INT(la, ha), INT(lb, hb)) => INT(min(la, lb), max(ha, hb))

      case (t, o) => o join t
    }
  }

  private def intOp(
      fun: (BigInt, BigInt, BigInt, BigInt) => (BigInt, BigInt),
      t: InterVal,
      o: InterVal
    ): InterVal = {
    (t, o) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()
      case (TOP(), _) | (_, TOP()) => TOP()
      case (INT(la, ha), INT(lb, hb)) => {
        val (l, h) = fun(la, ha, lb, hb)
        INT(l, h)
      }
    }
  }

  override def add(other: InterVal): InterVal = {
    (this, other) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()
      case (TOP(), _) | (_, TOP()) => TOP()

      case (LEQ(_), GEQ(_)) => TOP()
      case (LEQ(ha), LEQ(hb)) => LEQ(ha + hb)
      case (GEQ(la), GEQ(lb)) => GEQ(la + lb)
      case (GEQ(la), INT(lb, _)) => GEQ(la + lb)
      case (LEQ(ha), INT(_, hb)) => LEQ(ha + hb)

      case (INT(la, ha), INT(lb, hb)) => {
        val l = la + lb
        val h = ha + hb
        INT(l, h)
      }

      case (t, o) => o add t
    }
  }

  private def invert(): InterVal = {
    this match {
      case INT(l, h) => GZ(-h, -l)
      case LEQ(x) => GEQ(-x)
      case GEQ(x) => LEQ(-x)
      case v => v
    }
  }

  override def sub(other: InterVal): InterVal = this add (other.invert())

  override def mul(other: InterVal): InterVal = //TODO

  override def div(other: InterVal): InterVal = //TODO

  override def slt(other: InterVal): InterVal = {
    (this, other) match {
      case (BOT(), _) => BOT()
      case (_, BOT()) => BOT()

      case (LEQ(ha), GEQ(lb)) if ha < lb => INT(1, 1)
      case (LEQ(ha), INT(lb, hb)) if ha < lb => INT(1, 1)
      case (INT(la, ha), INT(lb, hb)) if ha < lb => INT(1, 1)

      case (GEQ(la), LEQ(hb)) if la >= hb => INT(0, 0)
      case (INT(la, ha), LEQ(hb)) if la >= hb => INT(0, 0)
      case (INT(la, ha), INT(lb, hb)) if la >= hb => INT(0, 0)

      case _ => INT(0, 1)
    }
  }
    constOp((x, y) => if (x < y) 1 else 0, this, other)

  override def canBeZero(): Boolean = {
    this match {
      case TOP() => true
      case INT(l, h) => (l <= 0 && h >= 0)
      case _ => false
    }
  }

  override def canBeNonZero(): Boolean = {
    this match {
      case TOP() => true
      case INT(l, h) => (h < 0 || l > 0)
      case _ => false
    }
  }
}

case class BOT() extends InterVal
case class INT(l: BigInt, h: BigInt) extends InterVal
case class LEQ(h: BigInt) extends InterVal
case class GEQ(l: BigInt) extends InterVal
case class TOP() extends InterVal

/**
 * An implementation of the Sparse Conditional Intervals (SCI) analysis (i.e.
 * using reachability for more precise intervals.
 */
class SCIAnalysis(fun: Function)
  extends ConditionalValueAnalysis[InterVal](fun) {
  override def bot(): InterVal = BOT()

  override def fromBigInt(x: BigInt): InterVal = INT(x,x)
}

