package interpreter.impl

import interpreter.{Global, Ip}
import models.{GlobalVariable, IntValue}

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
    run(ctx)(text)
    ctx.getGlobal(GlobalVariable("SIGNAL")) should be(IntValue(3))
    ctx.ip should be(Ip(fakeRoutine, 1))
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
    run(ctx)(text)
    ctx.getGlobal(GlobalVariable("SIGNAL")) should be(IntValue(5))
    ctx.ip should be(Ip(fakeRoutine, 1))
  }

  it should "run no block if no condition is true" in new Env0 {
    val text =
      """
        |<COND (<RFALSE> <SETG SIGNAL 1>)
        |      (<RFALSE> <SETG SIGNAL 2>)
        |      (<RFALSE> <SETG SIGNAL 3>)
        |      (<RFALSE> <SETG SIGNAL 4>)
        |>""".stripMargin
    run(ctx)(text)
    ctx.getGlobal(GlobalVariable("SIGNAL")) should be(IntValue(0))
    ctx.ip should be(Ip(fakeRoutine, 1))
  }

  it should "work with more than one action" in new Env0 {
    val text =
      """
        |<COND (<RFALSE> <SETG SIGNAL 1>)
        |      (<RFALSE> <SETG SIGNAL 2>)
        |      (<RTRUE>  <SETG SIGNAL 7 ><SETG SIGNAL 8>)
        |      (<RTRUE>  <SETG SIGNAL 4>)
        |      (T        <SETG SIGNAL 5>)
        |>""".stripMargin
    run(ctx)(text)
    ctx.getGlobal(GlobalVariable("SIGNAL")) should be(IntValue(8))
    ctx.ip should be(Ip(fakeRoutine, 1))
  }
}
