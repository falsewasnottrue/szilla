package models

import org.scalatest.{FlatSpec, Matchers}

class RoutineSpec extends FlatSpec with Matchers {

  "Routine" should "allow to add arguments" in {
    val r = Routine("ROUTINE-A")
    r.arguments should be(Nil)

    val r1 = r.withArgument(SimpleArgument("A"))
    r1.arguments should be(Seq(SimpleArgument("A")))
    val r2 = r1.withArgument(AuxArgument("B"))
    r2.arguments should be(Seq(SimpleArgument("A"), AuxArgument("B")))
    val r3 = r2.withArgument(OptArgument("C"))
    r3.arguments should be(Seq(SimpleArgument("A"), AuxArgument("B"), OptArgument("C")))
  }

  it should "allow to add instructions" in {
    val r = Routine("ROUTINE-A")
    r.instructions should be(Nil)

    val r1 = r.withInstruction(Instruction(ADD))
    r1.instructions should be(Seq(Instruction(ADD)))
    val r2 = r1.withInstruction(Instruction(QUIT))
    r2.instructions should be(Seq(Instruction(ADD), Instruction(QUIT)))
  }
}
