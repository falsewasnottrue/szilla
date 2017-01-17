package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, IntValue}
import org.scalatest.{FlatSpec, Matchers}

class MulInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "MulInterpreter" should "multiply two ints" in new Env {
    val instruction = Instruction.parser.parse("<* 6 7>")
    Interpreter.evaluate(ctx)(instruction)
    ctx.pop should be(Some(IntValue(42)))
  }

  it should "multiply multiple integers" in new Env {
    val add = Instruction.parser.parse("<MUL 1 2 3 4>")
    val res = Interpreter.evaluate(ctx)(add)
    res.pop should be(Some(IntValue(24)))
  }

  it should "fail for non-int operand" in new Env {
    val add = Instruction.parser.parse("<MUL 1 \"bad\">")
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(add)
    }
  }
}
