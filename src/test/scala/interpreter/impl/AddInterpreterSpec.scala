package interpreter.impl

import interpreter.{Context, Interpreter}
import models._
import org.scalatest.{FlatSpec, Matchers}

class AddInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "AddInterpreter" should "add two integers" in new Env {
    val add = Instruction.parser.parse("<+ 19 23>")
    val res = Interpreter.evaluate(ctx)(add)
    res.pop should be(Some(IntValue(42)))
  }

  it should "add multiple integers" in new Env {
    val add = Instruction.parser.parse("<ADD 1 2 3 4>")
    val res = Interpreter.evaluate(ctx)(add)
    res.pop should be(Some(IntValue(10)))
  }

  it should "fail for non-int operand" in new Env {
    val add = Instruction.parser.parse("<ADD 1 \"abc\">")
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(add)
    }
  }
}
