package interpreter.impl

import models.{Properties, PropertyName}
import org.mockito.Mockito

class PrintDInterpreterSpec extends BaseInterpreterSpec {

  trait Env0 extends Env {
    val lantern = createObject(ctx)("LANTERN", Properties().add(PropertyName.DESC, "brass lantern"))
    val livingRoom = createRoom(ctx)("LIVING-ROOM", Properties().add(PropertyName.DESC, "living room"))
  }

  "PrintInterpreter" should "print the DESC of the given object." in new Env0 {
    val ctxSpy = Mockito.spy(ctx)

    println(lantern)
    run(ctxSpy)("<PRINTD ,LANTERN>")

    Mockito.verify(ctxSpy).out("brass lantern")
  }

  it should "print the DESC of the given room." in new Env0 {
    val ctxSpy = Mockito.spy(ctx)
    run(ctxSpy)("<PRINTD ,LIVING-ROOM>")

    Mockito.verify(ctxSpy).out("living room")
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
