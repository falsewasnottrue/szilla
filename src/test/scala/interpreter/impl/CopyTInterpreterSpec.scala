package interpreter.impl

import interpreter.Global
import models._

import scala.collection.mutable

class CopyTInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val table = TableValue(mutable.Map(0 -> BoolValue(true), 1 -> StringValue("abc"), 2 -> IntValue(42)))
    Global.define(GlobalVariable("CURRENT-MOVE-TBL"), table)
    val tableDest = TableValue()
    Global.define(GlobalVariable("OLD-MOVE-TBL"), tableDest)
  }

  "CopyTInterpreter" should "copy table1 into table2" in new Env0 {
    run(ctx)("<COPYT ,CURRENT-MOVE-TBL ,OLD-MOVE-TBL 2>")
    tableDest.v.get(0) should be(Some(BoolValue(true)))
    tableDest.v.get(1) should be(Some(StringValue("abc")))
    tableDest.v.get(2) should be(None)
  }

  it should "fail for too many arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<COPYT ,CURRENT-MOVE-TBL ,OLD-MOVE-TBL 2 <RFALSE>>")
    }
  }

  it should "fail for too few arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<COPYT ,CURRENT-MOVE-TBL ,OLD-MOVE-TBL>")
    }
  }

  it should "fail for wrong type of arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<COPYT 2 ,CURRENT-MOVE-TBL ,OLD-MOVE-TBL>")
    }
  }
}
