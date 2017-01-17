package interpreter.impl

import models.BoolValue

class EqualQInterpreterSpec extends BaseInterpreterSpec {

  "EqualQInterpreter" should "test integers for equality" in new Env {
    run(ctx)("<EQUAL? 42 42>").pop should be(Some(BoolValue(true)))
  }

  it should "test multiple integers for equality" in new Env {
    run(ctx)("<EQUAL? 42 23 42 17>").pop should be(Some(BoolValue(true)))
  }

  it should "test integers for inequality" in new Env {
    run(ctx)("<EQUAL? 42 23>").pop should be(Some(BoolValue(false)))
  }

  it should "test multiple integers for inequality" in new Env {
    run(ctx)("<EQUAL? 42 23 <RTRUE> \"bad\">").pop should be(Some(BoolValue(false)))
  }

  it should "test booleans for equality" in new Env {
    run(ctx)("<EQUAL? <RTRUE> <RTRUE>>").pop should be(Some(BoolValue(true)))
  }

  it should "test multiple booleans for equality" in new Env {
    run(ctx)("<EQUAL? <RTRUE> <RFALSE> <RTRUE> 42>").pop should be(Some(BoolValue(true)))
  }

  it should "test booleans for inequality" in new Env {
    run(ctx)("<EQUAL? <RTRUE> <RFALSE>>").pop should be(Some(BoolValue(false)))
  }

  it should "test multiple booleans for inequality" in new Env {
    run(ctx)("<EQUAL? <RTRUE> <RFALSE> <RFALSE> 42>").pop should be(Some(BoolValue(false)))
  }

  it should "test strings for equality" in new Env {
    run(ctx)("<EQUAL? \"abc\" \"abc\">").pop should be(Some(BoolValue(true)))
  }

  it should "test multiple strings for equality" in new Env {
    run(ctx)("<EQUAL? \"abc\" \"bad\" \"abc\" 42>").pop should be(Some(BoolValue(true)))
  }

  it should "test strings for inequality" in new Env {
    run(ctx)("<EQUAL? \"abc\" \"bad\">").pop should be(Some(BoolValue(false)))
  }

  it should "test multiple strings for inequality" in new Env {
    run(ctx)("<EQUAL? \"abc\" \"bad\" 42 <RTRUE>>").pop should be(Some(BoolValue(false)))
  }

  it should "fail with too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<EQUAL?>")
    }
  }
}
