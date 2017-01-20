package interpreter.impl

class FClearInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val diamond = createObject(ctx)("GUARDED-DIAMOND")
    val tryTakeBit = createFlag(ctx)("TRYTAKEBIT")
    diamond.addFlag(tryTakeBit)

    val stable = createRoom(ctx)("STABLE")
    val burningBit = createFlag(ctx)("BURNING-BIT")
    stable.addFlag(burningBit)
  }

  "FClearInterpreter" should "clear flags in an object" in new Env0 {
    run(ctx)("<FCLEAR ,GUARDED-DIAMOND ,TRYTAKEBIT>")
    diamond.flags.contains(tryTakeBit) should be(false)
  }

  it should "clear flags in a room" in new Env0 {
    run(ctx)("<FCLEAR ,STABLE ,BURNING-BIT>")
    stable.flags.contains(burningBit) should be(false)
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FCLEAR ,SECRET-WILL ,DESK <RTRUE>>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FCLEAR ,BRASS-LAMP>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FCLEAR 42 <RTRUE>>")
    }
  }
}
