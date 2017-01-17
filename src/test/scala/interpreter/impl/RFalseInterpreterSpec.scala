package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{BoolValue, Instruction}
import org.scalatest.{FlatSpec, Matchers}

class RFalseInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "RFalseInterpreter" should "return false" in new Env {
    val i = Instruction.parser.parse("<RFALSE>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "fail with parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<RFALSE 1>"))
    }
  }
}
