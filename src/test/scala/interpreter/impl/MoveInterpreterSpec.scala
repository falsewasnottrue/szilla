package interpreter.impl

import models._

class MoveInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val bread = createObject(ctx)("BREAD")
    val toaster = createObject(ctx)("TOASTER")
    val livingRoom = createRoom(ctx)("LIVING-ROOM")
  }

  "MoveInterpreter" should "put an object into another" in new Env0 {
    run(ctx)("<MOVE ,BREAD ,TOASTER>")

    toaster.contains(bread) should be(true)
    bread.location should be(RefLocation(toaster.id))
  }

  it should "put an object into a room" in new Env0 {
    run(ctx)("<MOVE ,BREAD ,LIVING-ROOM>")
    livingRoom.contains(bread) should be(true)
    bread.location should be(RefLocation(livingRoom.id))
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
