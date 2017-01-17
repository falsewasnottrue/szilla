package interpreter.impl

import models.{Object, ObjectValue, Variable}

class MoveInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val breadVar = Variable(",BREAD")
    val toasterVar = Variable(",TOASTER")
    val bread = Object("bread")
    val toaster = Object("toaster")

    ctx.set(breadVar, ObjectValue(bread))
    ctx.set(toasterVar, ObjectValue(toaster))
  }

  "MoveInterpreter" should "put an object into another" in new Env0 {
    toaster.contains(bread) should be(false)
    run(ctx)("<MOVE ,BREAD ,TOASTER>")

    toaster.contains(bread) should be(true)
  }

  it should "fail with too many parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<MOVE ,BREAD ,TOASTER 1>")
    }
  }

  it should "fail with too few parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<MOVE ,BREAD>")
    }
  }

  it should "fail with wrong type of parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<MOVE ,BREAD \"bad\">")
    }
  }
}
