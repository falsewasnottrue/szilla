package interpreter.impl

import models.IntValue

class ModInterpreterSpec extends BaseInterpreterSpec {

  "ModInterpreter" should "calculate rest of division of two ints" in new Env {
    run(ctx)("<MOD 10 3>").pop should be(Some(IntValue(1)))
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<MOD 10 3 1>")
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<MOD 10>")
    }
  }

  it should "fail with wrong type of parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<MOD 10 \"bad\">")
    }
  }
}