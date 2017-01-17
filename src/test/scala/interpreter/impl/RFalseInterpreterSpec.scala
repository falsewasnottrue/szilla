package interpreter.impl

import models.BoolValue

class RFalseInterpreterSpec extends BaseInterpreterSpec {

  "RFalseInterpreter" should "return false" in new Env {
    run(ctx)("<RFALSE>").pop should be(Some(BoolValue(false)))
  }

  it should "fail with parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<RFALSE 1>")
    }
  }
}
