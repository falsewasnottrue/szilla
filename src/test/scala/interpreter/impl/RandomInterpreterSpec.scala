package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, IntValue}
import org.scalatest.{FlatSpec, Matchers}

class RandomInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "RandomInterpreter" should "create a random number" in new Env {
    val text = "<RANDOM 100>"
    Interpreter.evaluate(ctx)(Instruction.parser.parse(text))
    val Some(IntValue(res)) = ctx.pop
    res should (be >= 1 and be <= 100)
  }

  it should "fail with more than one parameter" in new Env {
    intercept[IllegalStateException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<RANDOM 1 2 3>"))
    }
  }

  it should "fail with no parameters" in new Env {
    intercept[IllegalStateException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<RANDOM>"))
    }
  }

  it should "fail with the wrong type of parameter" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<RANDOM \"bad\">"))
    }
  }
}
