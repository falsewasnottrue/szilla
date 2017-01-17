package interpreter.impl

import models.IntValue

class MulInterpreterSpec extends BaseInterpreterSpec {

  "MulInterpreter" should "multiply two ints" in new Env {
    run(ctx)("<* 6 7>").pop should be(Some(IntValue(42)))
  }

  it should "multiply multiple integers" in new Env {
    run(ctx)("<MUL 1 2 3 4>").pop should be(Some(IntValue(24)))
  }

  it should "fail for non-int operand" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<MUL 1 \"bad\">")
    }
  }
}
