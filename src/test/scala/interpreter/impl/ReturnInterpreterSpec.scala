package interpreter.impl

import interpreter.{Context, InstructionPointer}
import models.{Block, BoolValue}

class ReturnInterpreterSpec extends BaseInterpreterSpec {

  "Return interpreter" should "escape to surrounding scope and return single result" in new Env {
    val c = new Context(InstructionPointer(fakeRoutine, 0), parent = Some(ctx))
    val res = run(c)("<RETURN <RTRUE>>")

    res should be(ctx)
    res.pop should be(Some(BoolValue(true)))
  }

  it should "escape scope without returning a result" in new Env {
    val c = new Context(InstructionPointer(Block(Seq())), parent = Some(ctx))
    val res = run(c)("<RETURN>")

    res should be(ctx)
    res.pop should be(None)
  }

  it should "fail if there is no surrounding scope" in new Env {
    intercept[IllegalStateException] {
      run(ctx)("<RETURN 1>")
    }
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      val c = new Context(InstructionPointer(fakeRoutine, 0), parent = Some(ctx))
      run(c)("<RETURN 1 2>")
    }
  }
}
