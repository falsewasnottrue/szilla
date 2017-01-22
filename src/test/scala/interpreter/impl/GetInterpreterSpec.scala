package interpreter.impl

import interpreter.Global
import models._

class GetInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val table = TableValue(Map(0 -> BoolValue(true), 1 -> StringValue("abc"), 2 -> IntValue(42)))
    Global.set(Variable(",MAZE-EXITS"), table)
  }

  "GetInterpreter" should "return the value that is stored in the ith slot in the given table" in new Env0 {
    run(ctx)("<GET ,MAZE-EXITS 1>").pop should be(Some(StringValue("abc")))
  }

  it should "return false as default value" in new Env0 {
    run(ctx)("<GET ,MAZE-EXITS 42>").pop should be(Some(BoolValue(false)))
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<GET ,MAZE-EXITS 1 <RTRUE>>")
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<GET ,MAZE-EXITS>")
    }
  }

  it should "fail with wrong type of parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<GET 1 ,MAZE-EXITS>")
    }
  }
}
