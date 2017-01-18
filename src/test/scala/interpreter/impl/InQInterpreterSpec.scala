package interpreter.impl

import interpreter.Global
import models.{BoolValue, Object, ObjectValue, Variable}

class InQInterpreterSpec extends BaseInterpreterSpec{

  trait Env0 extends Env {
    val will = createObject(ctx)("SECRET-WILL")
    val safe = createObject(ctx)("WALL-SAFE")
    val desk = createObject(ctx)("DESK")

    safe.insert(will)
  }

  "InQInterpreter" should "returns true if object2 is the LOC of object1" in new Env0 {
    run(ctx)("<IN? ,SECRET-WILL ,WALL-SAFE>").pop should be(Some(BoolValue(true)))
  }

  it should "return false if object2 is not the LOC of object1" in new Env0 {
    run(ctx)("<IN? ,SECRET-WILL ,DESK>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<IN? ,SECRET-WILL ,DESK <RTRUE>>")
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
