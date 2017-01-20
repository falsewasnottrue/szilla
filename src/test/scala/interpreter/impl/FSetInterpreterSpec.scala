package interpreter.impl

class FSetInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val torch = createObject(ctx)("OILY-TORCH")
    val flameBit = createFlag(ctx)("FLAMEBIT")
    val stable = createRoom(ctx)("STABLE")
  }

  "FSetInterpreter" should "sets flag1 in object1." in new Env0 {
    run(ctx)("<FSET ,OILY-TORCH ,FLAMEBIT>")
    torch.flags.contains(flameBit) should be(true)
  }

  it should "set flag1 on a room" in new Env0 {
    run(ctx)("<FSET ,STABLE ,FLAMEBIT>")
    stable.flags.contains(flameBit) should be(true)
  }

  it should "fail for too many arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FSET ,SECRET-WILL ,DESK <RTRUE>>")
    }
  }

  it should "fail for too few arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FSET ,BRASS-LAMP>")
    }
  }

  it should "fail for wrong type of arguments" in new Env {
    intercept[IllegalArgumentException] {
      run(ctx)("<FSET 42 <RTRUE>>")
    }
  }
}
