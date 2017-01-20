package interpreter.impl

import models.{BoolValue, Properties, PropertyName, StringValue}

class GetPInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val here = createRoom(ctx)("HERE", Properties().add(PropertyName.DESC, "here we are"))
    val torch = createObject(ctx)("TORCH", Properties().add(PropertyName.LDESC, "this is a torch"))
  }

  "GetPInterpreter" should "return the specified property of an object" in new Env0 {
    run(ctx)("<GETP ,TORCH ,P?LDESC>").pop should be(Some(StringValue("this is a torch")))
  }

  it should "return the specified property of a room" in new Env0 {
    run(ctx)("<GETP ,HERE ,P?DESC>").pop should be(Some(StringValue("here we are")))
  }

  it should "return false for an unknown property" in new Env0 {
    run(ctx)("<GETP ,HERE ,P?GIBSNICH>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<GETP ,SECRET-WILL ,DESK <RTRUE>>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<GETP ,BRASS-LAMP>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<GETP 42 <RTRUE>>")
    }
  }
}
