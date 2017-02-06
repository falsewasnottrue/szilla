package interpreter.impl

import models.BoolValue

class NotInterpreterSpec extends BaseInterpreterSpec {

  "NotInterpreter" should "calculate negation" in new Env {
    run(ctx)("<NOT <RTRUE>>").pop should be(Some(BoolValue(false)))
    run(ctx)("<NOT <RFALSE>>").pop should be(Some(BoolValue(true)))
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<NOT <RTRUE> <RFALSE>>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<NOT>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<NOT 42>")
    }
  }

}
