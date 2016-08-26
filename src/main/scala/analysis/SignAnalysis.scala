package analysis

import cfg._

trait AbstractVal[A] {
  def join(other: A): A

  def add(other: A): A
  def sub(other: A): A
  // def mul(other: A): A
  // def div(other: A): A

  def slt(other: A): A

  def canBeZero(): Boolean
  def canBeNonZero(): Boolean
}

sealed abstract class SignVal extends AbstractVal[SignVal] {
  override def join(other: SignVal): SignVal = {
    (this, other) match {
      case (TOP(), _) | (_, TOP()) => TOP()
      case (BOT(), o) => o
      case (t, BOT()) => t
      case (t, o) if t == o => t

      case (EZ(), GZ()) => GEZ()
      case (EZ(), LZ()) => LEZ()
      case (EZ(), LEZ()) => LEZ()
      case (EZ(), GEZ()) => GEZ()
      case (EZ(), NEZ()) => TOP()

      case (GZ(), LZ()) => NEZ()
      case (GZ(), LEZ()) => TOP()
      case (GZ(), GEZ()) => GEZ()
      case (GZ(), NEZ()) => NEZ()

      case (LZ(), LEZ()) => LEZ()
      case (LZ(), GEZ()) => TOP()
      case (LZ(), NEZ()) => NEZ()

      case (LEZ(), GEZ()) => TOP()
      case (LEZ(), NEZ()) => TOP()

      case (GEZ(), NEZ()) => TOP()

      case (t, o) => o join t
    }
  }

  def add(other: SignVal): SignVal = {
    (this, other) match {
      case (TOP(), _) | (_, TOP()) => TOP()
      case (BOT(), o) => BOT()
      case (t, BOT()) => BOT()
      case (NEZ(), NEZ()) => TOP()
      case (t, o) if t == o => t

      case (EZ(), GZ()) => GZ()
      case (EZ(), LZ()) => LZ()
      case (EZ(), LEZ()) => LEZ()
      case (EZ(), GEZ()) => GEZ()
      case (EZ(), NEZ()) => NEZ()

      case (GZ(), LZ()) => TOP()
      case (GZ(), LEZ()) => TOP()
      case (GZ(), GEZ()) => GZ()
      case (GZ(), NEZ()) => TOP()

      case (LZ(), LEZ()) => LZ()
      case (LZ(), GEZ()) => TOP()
      case (LZ(), NEZ()) => TOP()

      case (LEZ(), GEZ()) => TOP()
      case (LEZ(), NEZ()) => TOP()

      case (GEZ(), NEZ()) => TOP()

      case (t, o) => o add t
    }
  }

  private def invert(): SignVal = {
    this match {
      case LZ() => GZ()
      case GZ() => LZ()
      case LEZ() => GEZ()
      case GEZ() => LEZ()
      case s => s
    }
  }

  def sub(other: SignVal): SignVal = this add (other.invert())

  def slt(other: SignVal): SignVal =
    if ((this sub other) == LZ()) GZ() else EZ()

  override def canBeZero(): Boolean = (this join NEZ()) == TOP()

  override def canBeNonZero(): Boolean = (this join EZ()) != EZ()
}

case class BOT() extends SignVal
case class EZ() extends SignVal
case class GZ() extends SignVal
case class LZ() extends SignVal
case class LEZ() extends SignVal
case class GEZ() extends SignVal
case class NEZ() extends SignVal
case class TOP() extends SignVal

/**
 * A simple sign analysis that determines the possible signs of all occuring
 * values in the function.
 */
class SignAnalysis(fun: Function) extends ValueAnalysis[SignVal](fun) {


  override def run(): Unit = {
    populateSymbolTable(new BOT())

  }

  override def fromBigInt(x: BigInt): SignVal = {
    if (x > 0) {
      GZ()
    } else if (x < 0) {
      LZ()
    } else {
      EZ()
    }
  }
}
