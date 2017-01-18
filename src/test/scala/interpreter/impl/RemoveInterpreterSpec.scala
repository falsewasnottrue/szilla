package interpreter.impl

import interpreter.Context
import models.{Empty, RefLocation}

class RemoveInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val bread = createObject(ctx)("BREAD")
    val toaster = createObject(ctx)("TOASTER")
    val livingRoom = createRoom(ctx)("LIVING-ROOM")
  }

  "RemoveInterpreter" should "removes an object from another object, setting its LOC to false" in new Env0 {
    toaster.insert(bread)
    toaster.contains(bread) should be(true)
    bread.location should be(RefLocation(toaster.id))

    run(ctx)("<REMOVE ,BREAD>")
    toaster.contains(bread) should be(false)
    bread.location should be(Empty)
  }

  "RemoveInterpreter" should "removes an object from a room, setting its LOC to false" in new Env0 {
    livingRoom.insert(bread)
    livingRoom.contains(bread) should be(true)
    bread.location should be(RefLocation(livingRoom.id))

    run(ctx)("<REMOVE ,BREAD>")
    livingRoom.contains(bread) should be(false)
    bread.location should be(Empty)
  }


  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<REMOVE ,BREAD 1>")
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<REMOVE>")
    }
  }

  it should "fail with the wrong kind of parameters" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<REMOVE 1>")
    }
  }
}
