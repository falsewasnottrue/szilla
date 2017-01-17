package interpreter.impl

import models.BoolValue

class RTrueInterpreterSpec extends BaseInterpreterSpec {

  "RTrueInterpreter" should "return true" in new Env {
    run(ctx)("<RTRUE>").pop should be(Some(BoolValue(true)))
  }

  it should "fail with parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<RTRUE 1>")
    }
  }
}
