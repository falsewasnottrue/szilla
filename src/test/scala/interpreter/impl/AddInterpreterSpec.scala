package interpreter.impl

import interpreter.{Context, Global, Ip}
import models._

class AddInterpreterSpec extends BaseInterpreterSpec {

  "AddInterpreter" should "add two integers" in new Env {
    run(ctx)("<+ 19 23>").pop should be(Some(IntValue(42)))
  }

  it should "add multiple integers" in new Env {
    run(ctx)("<ADD 1 2 3 4>").pop should be(Some(IntValue(10)))
  }

  it should "advance the ip by default" in new Env {
    run(ctx)("<+ 19 23>").ip should be(Ip(fakeRoutine, 1))
  }

  it should "work with global variables" in new Env {
    Global.define(GlobalVariable("NUM"), IntValue(23))
    run(ctx)("<+ 19 ,NUM>").pop should be(Some(IntValue(42)))
  }

  it should "work with local variables" in new Env {
    ctx.set(LocalVariable("NUM"), IntValue(23))

    run(ctx)("<+ 19 .NUM>").pop should be(Some(IntValue(42)))
  }

  it should "fail for non-int operand" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<ADD 1 \"abc\">")
    }
  }
}
