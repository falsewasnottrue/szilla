package interpreter.impl

import models._

class FSetQInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val lamp = Object("BRASS-LAMP").withFlag(Flag("ONBIT"))
    val lampVar = Variable(",BRASS-LAMP")

    ctx.set(lampVar, ObjectValue(lamp))

    ctx.set(Variable(",ONBIT"), StringValue("ONBIT"))
    ctx.set(Variable(",SOMEBIT"), StringValue("SOMEBIT"))
  }

  "FSetQInterpreter" should "return true if bit is set" in new Env0 {
    run(ctx)("<FSET? ,BRASS-LAMP ,ONBIT>").pop should be(Some(BoolValue(true)))
  }

  it should "return false otherwise" in new Env0 {
    run(ctx)("<FSET? ,BRASS-LAMP ,SOMEBIT>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FSET? ,BRASS-LAMP ,SOMEBIT <RTRUE>>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FSET? ,BRASS-LAMP>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FSET? 42 <RTRUE>>")
    }
  }
}
