package models

import org.scalatest.{FlatSpec, Matchers}

class InstructionSpec extends FlatSpec with Matchers {

  "Instruction" should "allow to add operands" in {
    val i = Instruction(ADD)
    i.operands should be(Nil)

    val i2 = i.withOperand(",a")
    i2.operands should be(Seq(",a"))
    val i3 = i2.withOperand(",b")
    i3.operands should be(Seq(",a", ",b"))
  }
}
