package interpreter.impl

import models._

class AddInterpreterSpec extends BaseInterpreterSpec {

  "AddInterpreter" should "add two integers" in new Env {
    run(ctx)("<+ 19 23>").pop should be(Some(IntValue(42)))
  }

  it should "add multiple integers" in new Env {
    run(ctx)("<ADD 1 2 3 4>").pop should be(Some(IntValue(10)))
  }

  it should "fail for non-int operand" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<ADD 1 \"abc\">")
    }
  }
}
