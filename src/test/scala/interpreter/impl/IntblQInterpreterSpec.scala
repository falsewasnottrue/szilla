package interpreter.impl

import interpreter.Global
import models._

import scala.collection.mutable

class IntblQInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val rudolph = createObject(ctx)("RUDOLPH")
    val table = TableValue(mutable.Map(0 -> BoolValue(true), 1 -> StringValue("abc"), 2 -> IntValue(42), 17 -> RefValue("RUDOLPH")))
    Global.define(GlobalVariable("REINDEER-TABLE"), table)
  }

  "IntblQInterpreter" should "return true if thing is found within the given table" in new Env0 {
    run(ctx)("<INTBL? ,RUDOLPH ,REINDEER-TABLE 8>").pop should be(Some(BoolValue(true)))
  }

  it should "return false if thing is not found" in new Env0 {
    run(ctx)("<INTBL? 23 ,REINDEER-TABLE 8>").pop should be(Some(BoolValue(false)))
  }

  it should "fail with too many parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<INTBL? ,RUDOLPH ,REINDEER-TABLE 8 <RTRUE>>")
    }
  }

  it should "fail with too few parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<INTBL? ,RUDOLPH ,REINDEER-TABLE>")
    }
  }

  it should "fail with wrong type of parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<INTBL? 8 ,RUDOLPH ,REINDEER-TABLE>")
    }
  }
}
