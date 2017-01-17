package interpreter.impl

import models.BoolValue

class ZeroQInterpreterSpec extends BaseInterpreterSpec {

  "ZeroQInterpreter" should "return true if parameter is 0" in new Env {
    run(ctx)("<ZERO? 0>").pop should be(Some(BoolValue(true)))
  }

  it should "return false if parameter is not 0" in new Env {
    run(ctx)("<ZERO? 42>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for more than one parameter" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<ZERO? 1 2>")
    }
  }

  it should "fail for less than one parameter" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<ZERO?>")
    }
  }

  it should "fail for wrong type of parameter" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<ZERO? \"bad\">")
    }
  }
}
