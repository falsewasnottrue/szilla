package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, IntValue}
import org.scalatest.{FlatSpec, Matchers}

class SubInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "SubInterpreter" should "subtract to ints" in new Env {
    val instruction = Instruction.parser.parse("<- 10 3>")
    Interpreter.evaluate(ctx)(instruction)
    ctx.pop should be(Some(IntValue(7)))
  }
}
