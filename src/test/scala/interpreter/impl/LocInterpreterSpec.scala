package interpreter.impl

import models.{BoolValue, RefValue}

class LocInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val livingRoom = createRoom("LIVING-ROOM")
    val smokingGun = createObject("SMOKING-GUN")
    val desk = createObject("DESK")
  }

  "LocInterpreterSpec" should "returns the location of an object" in new Env0 {
    desk.insert(smokingGun)
    run(ctx)("<LOC ,SMOKING-GUN>").pop should be(Some(RefValue("DESK")))
  }

  it should "returns the location of an object if it is room" in new Env0 {
    livingRoom.insert(smokingGun)
    run(ctx)("<LOC ,SMOKING-GUN>").pop should be(Some(RefValue("LIVING-ROOM")))
  }

  it should "return false if there is no LOC" in new Env0 {
    run(ctx)("<LOC ,SMOKING-GUN>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<LOC ,SMOKING-GUN 18>")
    }
  }

  it should "fail for too few arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<LOC>")
    }
  }

  it should "fail for wrong type of arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<LOC 42>")
    }
  }
}
