package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, IntValue}
import org.scalatest.{FlatSpec, Matchers}

class DivInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "DivInterpreter" should "divide to ints" in new Env {
    val instruction = Instruction.parser.parse("</ 10 3>")
    Interpreter.evaluate(ctx)(instruction)
    ctx.pop should be(Some(IntValue(3)))
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<DIV 10 3 1>"))
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("</ 10>"))
    }
  }

  it should "fail with wrong type of parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<DIV 10 \"bad\">"))
    }
  }
}