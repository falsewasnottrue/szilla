package interpreter.impl

import interpreter.Global
import models._
import org.mockito.Mockito

class TellInterpreterSpec extends BaseInterpreterSpec {

  "TellInterpreter" should "evaluate and print its operands" in new Env {
    Global.define(GlobalVariable("NAME"), StringValue("Anton"))
    val ctxSpy = Mockito.spy(ctx)

    run(ctxSpy)("<TELL \"Hallo \" ,NAME \"!\">")

    Mockito.verify(ctxSpy).out("Hallo ")
    Mockito.verify(ctxSpy).out("Anton")
    Mockito.verify(ctxSpy).out("!")
  }
}
