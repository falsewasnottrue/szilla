package interpreter.impl

import models.IntValue

class SubInterpreterSpec extends BaseInterpreterSpec {

  "SubInterpreter" should "subtract two ints" in new Env {
    run(ctx)("<- 10 3>").pop should be(Some(IntValue(7)))
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<- 10 3 1>")
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<- 10>")
    }
  }

  it should "fail with wrong type of parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<- 10 \"bad\">")
    }
  }
}
