package interpreter.impl

import models._

class PutPInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val tomato = createObject("ROTTING-TOMATO")
    val livingRoom = createRoom("LIVING-ROOM")
  }

  "PutPInterpreter" should "change the value of the given object's given property to thing" in new Env0 {
    run(ctx)("PUTP ,ROTTING-TOMATO ,P?SDESC \"rotten tomato\">")
    val Some(o: Object) = ctx.deref(tomato.id)
    o.properties.get("SDESC") should be(Some("rotten tomato"))
  }

  it should "change the value of the given rooms's given property to thing" in new Env0 {
    run(ctx)("<PUTP ,LIVING-ROOM ,P?DESC \"living room\">")
    val Some(r: Room) = ctx.deref(livingRoom.id)
    r.properties.get("DESC") should be(Some("living room"))
  }

  it should "fail with too many parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PUTP ,BREAD ,TOASTER 1 <RTRUE>>")
    }
  }

  it should "fail with too few parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PUTP ,BREAD>")
    }
  }

  it should "fail with wrong type of parameters" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PUTP \"bad\" ,BREAD 2>")
    }
  }
}
