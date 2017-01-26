package interpreter.impl

import models.{GlobalVariable, StringValue, Variable}
import org.mockito.Mockito

class TellInterpreterSpec extends BaseInterpreterSpec {

  "TellInterpreter" should "evaluate and print its operands" in new Env {
    ctx.setGlobal(GlobalVariable("NAME"), StringValue("Anton"))
    val ctxSpy = Mockito.spy(ctx)

    run(ctxSpy)("<TELL \"Hallo \" ,NAME \"!\">")

    Mockito.verify(ctxSpy).out("Hallo ")
    Mockito.verify(ctxSpy).out("Anton")
    Mockito.verify(ctxSpy).out("!")
  }
}
