package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, StringValue, Variable}
import org.mockito.Mockito
import org.scalatest.{FlatSpec, Matchers}

class TellInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "TellInterpreter" should "evaluate and print its operands" in new Env {
    val tell = Instruction.parser.parse("<TELL \"Hallo \" ,NAME \"!\">")
    ctx.set(Variable(",NAME"), StringValue("Anton"))
    val ctxSpy = Mockito.spy(ctx)

    Interpreter.evaluate(ctxSpy)(tell)
    Mockito.verify(ctxSpy).out("Hallo ")
    Mockito.verify(ctxSpy).out("Anton")
    Mockito.verify(ctxSpy).out("!")
  }
}
