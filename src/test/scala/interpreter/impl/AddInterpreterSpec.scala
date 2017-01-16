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

  it should "cast ints to double if necessary" in new Env {
    val add = Instruction.parser.parse("<+ 10 3.14>")
    val res = Interpreter.evaluate(ctx)(add)
    res.pop should be(Some(DoubleValue(13.14)))
  }

  it should "recursively evaluate its operands" in new Env {
    val add = Instruction.parser.parse("<+ <ADD 4 6> 3.14>")
    val res = Interpreter.evaluate(ctx)(add)
    res.pop should be(Some(DoubleValue(13.14)))
  }

  it should "fail if operand count is not 2" in new Env {
    val add = Instruction.parser.parse("<ADD 1 2 3 4>")
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(add)
    }
  }
}
