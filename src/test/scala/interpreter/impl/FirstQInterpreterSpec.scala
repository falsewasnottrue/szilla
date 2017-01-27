package interpreter.impl

import models.{BoolValue, RefValue}

class FirstQInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val smokingGun = createObject("SMOKING-GUN")
    val fryingPan = createObject("FRYING-PAN")
    val desk = createObject("DESK")
    val livingRoom = createRoom("LIVING-ROOM")
  }

  "FirstQInterpreter" should "return the first object within another object" in new Env0 {
    desk.insert(smokingGun)
    desk.insert(fryingPan)

    run(ctx)("<FIRST? ,DESK>").pop should be(Some(RefValue("SMOKING-GUN")))
  }

  it should "return the first object within a room" in new Env0 {
    livingRoom.insert(fryingPan)
    livingRoom.insert(smokingGun)

    run(ctx)("<FIRST? ,LIVING-ROOM>").pop should be(Some(RefValue("FRYING-PAN")))
  }

  it should "return the first if there is no object in another object" in new Env0 {
    run(ctx)("<FIRST? ,DESK>").pop should be(Some(BoolValue(false)))
  }

  it should "return the first if there is no object in a room" in new Env0 {
    run(ctx)("<FIRST? ,LIVING-ROOM>").pop should be(Some(BoolValue(false)))
  }

  it should "fail for too many arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<FIRST? ,DESK ,SMOKING-GUN>")
    }
  }

  it should "fail for too few arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<FIRST?>")
    }
  }

  it should "fail for wrong type of arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<FIRST? 42>")
    }
  }
}
