package interpreter.impl

import models._

import scala.collection.mutable

class ConstantInterpreterSpec extends BaseInterpreterSpec {

  "ConstantInterpreter" should "register a global constant" in new Env {
    run(ctx)("<CONSTANT FOUR 4>")
    ctx.getGlobal(GlobalVariable("FOUR")) should be(IntValue(4))
  }

  it should "work with GLOBAL keyword" in new Env {
    run(ctx)("<GLOBAL FUSE-COUNTER 0>")
    ctx.getGlobal(GlobalVariable("FUSE-COUNTER")) should be(IntValue(0))
  }

  it should "fail to redefine a global" in new Env {
    run(ctx)("<CONSTANT FOUR 3>")
    intercept[IllegalArgumentException] {
      run(ctx)("<CONSTANT FOUR 4>")
    }
  }

  it should "work for expressions" in new Env {
    run(ctx)("<CONSTANT MAZE-TABLE <TABLE 1 2 3>>")
    ctx.getGlobal(GlobalVariable("MAZE-TABLE")) should be(TableValue(
      mutable.Map(0 -> IntValue(1), 1 -> IntValue(2), 2 -> IntValue(3))
    ))
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<CONSTANT A 1 2>")
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<CONSTANT A>")
    }
  }

  it should "fail with wrong type of parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<CONSTANT 1 A>")
    }
  }
}
