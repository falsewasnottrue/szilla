package interpreter.impl

import models.BoolValue

class GrtrQInterpreterSpec extends BaseInterpreterSpec {

  "GrtrQInterpreter" should "return true if arg1 > arg2" in new Env {
    run(ctx)("<G? 42 23>").pop should be(Some(BoolValue(true)))
  }

  it should "return false otherwise" in new Env {
    run(ctx)("<GRTR? 23 42>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<G? 42 23 18>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<G? 42>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<G? 42 <RTRUE>>")
    }
  }
}
