package interpreter.impl

import models.BoolValue

class OrInterpreterSpec extends BaseInterpreterSpec {

  "OrInterpreter" should "calculate disjunction" in new Env {
    run(ctx)("<OR <RTRUE> <RTRUE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<OR <RTRUE> <RFALSE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<OR <RFALSE> <RTRUE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<OR <RFALSE> <RFALSE>>").pop should be(Some(BoolValue(false)))
  }

  it should "also work with several values" in new Env {
    run(ctx)("<OR <RTRUE> <RTRUE> <RTRUE> <RTRUE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<OR <RTRUE> <RTRUE> <RTRUE> <RFALSE>>").pop should be(Some(BoolValue(true)))
    run(ctx)("<OR <RFALSE> <RFALSE> <RFALSE> <RFALSE>>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<OR <RTRUE> \"abc\">")
    }
  }
}
