package interpreter.impl

import models.BoolValue

class LessQInterpreterSpec extends BaseInterpreterSpec {

  "LessQInterpreterSpec" should "return true if arg1 < arg2" in new Env {
    run(ctx)("<L? 23 42>").pop should be(Some(BoolValue(true)))
  }

  it should "return false otherwise" in new Env {
    run(ctx)("<LESS? 42 23>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<L? 42 23 18>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<L? 42>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<L? 42 <RTRUE>>")
    }
  }
}
