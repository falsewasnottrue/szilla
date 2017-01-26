package interpreter.impl

import interpreter.Global
import models._

import scala.collection.mutable

class SetGInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    Global.define(GlobalVariable("A"), IntValue(0))
  }

  "SetGInterpreter" should "set a global constant" in new Env0 {
    run(ctx)("<SETG A 4>")
    ctx.getGlobal(GlobalVariable("A")) should be(IntValue(4))
  }

  it should "fail to set an undefined a global" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<SETG B 3>")
    }
  }

  it should "work for expressions" in new Env0 {
    run(ctx)("<SETG A <TABLE 1 2 3>>")
    ctx.getGlobal(GlobalVariable("A")) should be(TableValue(
      mutable.Map(0 -> IntValue(1), 1 -> IntValue(2), 2 -> IntValue(3))
    ))
  }

  it should "fail with too many parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<SETG A 1 2>")
    }
  }

  it should "fail with too few parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<SETG A>")
    }
  }

  it should "fail with wrong type of parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<SETG 1 A>")
    }
  }
}
