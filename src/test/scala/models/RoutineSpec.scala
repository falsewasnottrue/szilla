package models

import org.scalatest.{FlatSpec, Matchers}

class RoutineSpec extends FlatSpec with Matchers {

  "Routine" should "allow to add arguments" in {
    val r = Routine("ROUTINE-A")
    r.arguments should be(Nil)

    val r1 = r.withArgument("A")
    r1.arguments should be(Seq("A"))
    val r2 = r1.withArgument("B")
    r2.arguments should be(Seq("A", "B"))
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
