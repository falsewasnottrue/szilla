package interpreter.impl

import org.mockito.Mockito

class PrintDInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    createObject(ctx)("LANTERN", desc = Some("brass lantern"))
    createRoom(ctx)("LIVING-ROOM", desc = Some("living room"))
  }

  "PrintInterpreter" should "print the DESC of the given object." in new Env0 {
    val ctxSpy = Mockito.spy(ctx)
    run(ctxSpy)("<PRINTD ,LANTERN>")

    Mockito.verify(ctxSpy).out("brass lantern")
  }

  it should "print the DESC of the given room." in new Env0 {
    val ctxSpy = Mockito.spy(ctx)
    run(ctxSpy)("<PRINTD ,LANTERN>")

    Mockito.verify(ctxSpy).out("brass lantern")
  }

  it should "fail for too many arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINTD ,LANTERN 123>")
    }
  }

  it should "fail for too few arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINTD>")
    }
  }

  it should "fail for wrong type of arguments" in new Env0 {
    intercept[IllegalArgumentException] {
      run(ctx)("<PRINTD \"Not bloody likely!\">")
    }
  }
}
