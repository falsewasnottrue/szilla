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
}
