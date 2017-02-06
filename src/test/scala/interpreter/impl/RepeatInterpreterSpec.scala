package interpreter.impl

import interpreter.{InstructionPointer, Global}
import models.{Block, GlobalVariable, Instruction, IntValue}

class RepeatInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    Global.define(GlobalVariable("A"), IntValue(0))
  }

  "RepeatInterpreter" should "run instructions in block" in new Env0 {
    val text =
      """
        |<REPEAT ()
        | <SETG A 1>
        | <RETURN>
        | <SETG A 2>
        |>
      """.stripMargin
    val c = run(ctx)(text)

    c.parent should be(Some(ctx))
    c.ip should be(InstructionPointer(Block(Seq(
      Instruction.parser.parse("<SETG A 1>"),
      Instruction.parser.parse("<RETURN>"),
      Instruction.parser.parse("<SETG A 2>")
    )), -1, true))
  }
}
