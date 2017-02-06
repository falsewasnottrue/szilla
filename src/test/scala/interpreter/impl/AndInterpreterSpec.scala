package interpreter.impl

import models.BoolValue

class AndInterpreterSpec extends BaseInterpreterSpec {

  "AndInterpreter" should "calculate conjunction" in new Env {
    run(ctx)("<AND <RTRUE> <RTRUE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<AND <RTRUE> <RFALSE>>").pop should be(Some(BoolValue(false)))
    run(ctx)("<AND <RFALSE> <RTRUE>>").pop should be(Some(BoolValue(false)))
    run(ctx)("<AND <RFALSE> <RFALSE>>").pop should be(Some(BoolValue(false)))
  }

  it should "also work with several values" in new Env {
    run(ctx)("<AND <RTRUE> <RTRUE> <RTRUE> <RTRUE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<AND <RTRUE> <RTRUE> <RTRUE> <RFALSE>>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<AND <RTRUE> \"abc\">")
    }
  }
}
