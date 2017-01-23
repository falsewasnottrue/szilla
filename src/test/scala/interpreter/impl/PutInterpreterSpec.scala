package interpreter.impl

import interpreter.Global
import models.{IntValue, RefValue, TableValue, Variable}


class PutInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val table = TableValue()
    Global.set(Variable(",SUSPECTS-TABLE"), table)
    Global.set(Variable(",SUSPECTS-POINTER"), IntValue(17))
    val butler = createObject(ctx)("BUTLER")
  }

  "PutInterpreterSpec" should "change the ith slot of the given table to thing" in new Env0 {
    run(ctx)("<PUT ,SUSPECTS-TABLE ,SUSPECTS-POINTER ,BUTLER>")
    table.v.get(17) should be(Some(RefValue("BUTLER")))
  }

  it should "fail with too many parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PUT ,SUSPECTS-TABLE ,SUSPECTS-POINTER ,BUTLER <RTRUE>>")
    }
  }

  it should "fail with too few parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PUT ,SUSPECTS-TABLE ,SUSPECTS-POINTER>")
    }
  }

  it should "fail with wrong type of parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PUT ,BUTLER ,SUSPECTS-TABLE ,SUSPECTS-POINTER>")
    }
  }
}
