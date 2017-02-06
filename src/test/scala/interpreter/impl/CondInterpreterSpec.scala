package interpreter.impl

import interpreter.{Global, InstructionPointer}
import models.{Block, GlobalVariable, Instruction, IntValue}

class CondInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    Global.define(GlobalVariable("SIGNAL"), IntValue(0))
  }

  "CondInterpreter" should "run first (and only first) block for which condition is true" in new Env0 {
    val text =
      """
        |<COND (<RFALSE> <SETG SIGNAL 1>)
        |      (<RFALSE> <SETG SIGNAL 2>)
        |      (<RTRUE>  <SETG SIGNAL 3>)
        |      (<RTRUE>  <SETG SIGNAL 4>)
        |      (T        <SETG SIGNAL 5>)
        |>""".stripMargin
    val c = run(ctx)(text)

    ctx.ip should be(InstructionPointer(fakeRoutine, 0))
    c.parent should be(Some(ctx))
    c.ip should be(InstructionPointer(Block(Seq(Instruction.parser.parse("<SETG SIGNAL 3>"))), -1))
  }

  it should "run default block if no condition is true" in new Env0 {
    val text =
      """
        |<COND (<RFALSE> <SETG SIGNAL 1>)
        |      (<RFALSE> <SETG SIGNAL 2>)
        |      (<RFALSE> <SETG SIGNAL 3>)
        |      (<RFALSE> <SETG SIGNAL 4>)
        |      (T        <SETG SIGNAL 5>)
        |>""".stripMargin
    val c = run(ctx)(text)
    ctx.ip should be(InstructionPointer(fakeRoutine, 0))
    c.parent should be(Some(ctx))
    c.ip should be(InstructionPointer(Block(Seq(Instruction.parser.parse("<SETG SIGNAL 5>"))), -1))
  }

  it should "run no block if no condition is true" in new Env0 {
    val text =
      """
        |<COND (<RFALSE> <SETG SIGNAL 1>)
        |      (<RFALSE> <SETG SIGNAL 2>)
        |      (<RFALSE> <SETG SIGNAL 3>)
        |      (<RFALSE> <SETG SIGNAL 4>)
        |>""".stripMargin
    val c = run(ctx)(text)
    ctx.ip should be(InstructionPointer(fakeRoutine, 0))
    c should be(ctx)
  }

  it should "work with more than one action" in new Env0 {
    val text =
      """
        |<COND (<RFALSE> <SETG SIGNAL 1>)
        |      (<RFALSE> <SETG SIGNAL 2>)
        |      (<RTRUE>  <SETG SIGNAL 7><SETG SIGNAL 8>)
        |      (<RTRUE>  <SETG SIGNAL 4>)
        |      (T        <SETG SIGNAL 5>)
        |>""".stripMargin
    val c = run(ctx)(text)

    ctx.ip should be(InstructionPointer(fakeRoutine, 0))
    c.parent should be(Some(ctx))
    c.ip should be(InstructionPointer(Block(Seq(Instruction.parser.parse("<SETG SIGNAL 7>"), Instruction.parser.parse("<SETG SIGNAL 8>"))), -1))
  }
}
