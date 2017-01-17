package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, IntValue}
import org.mockito.Mockito
import org.scalatest.{FlatSpec, Matchers}

class CRLFInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "CRLFInterpreter" should "print a carriage return" in new Env {
    val add = Instruction.parser.parse("<CRLF>")
    val spyCtx = Mockito.spy(ctx)
    Interpreter.evaluate(spyCtx)(add)

    Mockito.verify(spyCtx).out("\n")
  }

  it should "fail with parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<CRLF 1>"))
    }
  }
}