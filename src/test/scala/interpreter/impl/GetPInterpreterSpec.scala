package interpreter.impl

import models.{Properties, PropertyName, StringValue}

class GetPInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val here = createRoom(ctx)("HERE")
    val torch = createObject(ctx)("TORCH", Properties().add(PropertyName.LDESC, "TORCH LDESC"))
  }

  "GetPInterpreter" should "rReturns the specified property of an object" in new Env0 {
    // run(ctx)("<GETP ,TORCH ,P?LDESC>").pop should be(Some(StringValue("TORCH LDESC")))
  }

  // TODO for room
  // TODO unknown property

}
