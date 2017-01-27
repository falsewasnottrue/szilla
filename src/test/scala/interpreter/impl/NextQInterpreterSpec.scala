package interpreter.impl

import models.{BoolValue, RefValue}

class NextQInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val cabinet = createObject("KITCHEN-CABINET")
    val pitcher = createObject("PITCHER")
    val head = createObject("SEVERED-HEAD")
    cabinet.insert(pitcher)
    cabinet.insert(head)

    val kitchen = createRoom("KITCHEN")
    val knife = createObject("KNIFE")
    val sieve = createObject("SIEVE")
    kitchen.insert(knife)
    kitchen.insert(sieve)
  }

  "NextQInterpreter" should "return the next object in the linked contents of object1's LOC" in new Env0 {
    run(ctx)("<NEXT? ,PITCHER>").pop should be(Some(RefValue("SEVERED-HEAD")))
  }

  it should "return false if there is no next object" in new Env0 {
    run(ctx)("<NEXT? ,SEVERED-HEAD>").pop should be(Some(BoolValue(false)))
  }

  it should "return false if the object is not contained" in new Env0 {
    run(ctx)("<NEXT? ,KITCHEN-CABINET>").pop should be(Some(BoolValue(false)))
  }

  it should "return the next object in the linked contents of object1's LOC if it is in a room" in new Env0 {
    run(ctx)("<NEXT? ,KNIFE>").pop should be(Some(RefValue("SIEVE")))
  }

  it should "return false if there is no next object if it is in a room" in new Env0 {
    run(ctx)("<NEXT? ,SIEVE>").pop should be(Some(BoolValue(false)))
  }

  it should "return false if the object is a room" in new Env0 {
    run(ctx)("<NEXT? ,KITCHEN>").pop should be(Some(BoolValue(false)))
  }

 it should "fail for too many arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<NEXT? ,DESK ,SMOKING-GUN>")
    }
  }

  it should "fail for too few arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<NEXT?>")
    }
  }

  it should "fail for wrong type of arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<NEXT? 42>")
    }
  }

}
