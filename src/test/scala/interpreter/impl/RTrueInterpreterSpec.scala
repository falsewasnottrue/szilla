package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{BoolValue, Instruction}
import org.scalatest.{FlatSpec, Matchers}

class RTrueInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "RTrueInterpreter" should "return true" in new Env {
    val i = Instruction.parser.parse("<RTRUE>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "fail with parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<RTRUE 1>"))
    }
  }
}
