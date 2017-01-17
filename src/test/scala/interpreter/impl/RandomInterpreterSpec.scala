package interpreter.impl

import models.IntValue

class RandomInterpreterSpec extends BaseInterpreterSpec {

  "RandomInterpreter" should "create a random number" in new Env {
    run(ctx)("<RANDOM 100>")
    val Some(IntValue(res)) = ctx.pop
    res should (be >= 1 and be <= 100)
  }

  it should "fail with more than one parameter" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<RANDOM 1 2 3>")
    }
  }

  it should "fail with no parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<RANDOM>")
    }
  }

  it should "fail with the wrong type of parameter" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<RANDOM \"bad\">")
    }
  }
}
