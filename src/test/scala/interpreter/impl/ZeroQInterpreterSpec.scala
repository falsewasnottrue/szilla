package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{BoolValue, Instruction}
import org.scalatest.{FlatSpec, Matchers}

class ZeroQInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "ZeroQInterpreter" should "return true if parameter is 0" in new Env {
    val i = Instruction.parser.parse("<ZERO? 0>")
    Interpreter.evaluate(ctx)(i)

    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "return false if parameter is not 0" in new Env {
    val i = Instruction.parser.parse("<ZERO? 42>")
    Interpreter.evaluate(ctx)(i)

    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "fail for more than one parameter" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<ZERO? 1 2>"))
    }
  }

  it should "fail for less than one parameter" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<ZERO?>"))
    }
  }

  it should "fail for wrong type of parameter" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<ZERO? \"bad\">"))
    }
  }
}
