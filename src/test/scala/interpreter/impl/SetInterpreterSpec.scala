package interpreter.impl

import interpreter.{Context, InstructionPointer}
import models.{Block, IntValue, LocalVariable}

class SetInterpreterSpec extends BaseInterpreterSpec {

  "SetInterpreter" should "set local variables" in new Env {
    val c = run(ctx)("<SET A 1>")
    c.get(LocalVariable("A")) should be(IntValue(1))
  }

  it should "set the local variables in closest scope" in new Env {
    ctx.ip.scope should be(true)
    val c = new Context(InstructionPointer(Block(Seq())), Some(ctx))
    c.ip.scope should be(false)

    run(c)("<SET A 1>")
    c.get(LocalVariable("A")) should be(IntValue(1))
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<SET A 1 2>")
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<SET A>")
    }
  }
}
