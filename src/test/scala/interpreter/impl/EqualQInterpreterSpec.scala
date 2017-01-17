package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{BoolValue, Instruction}
import org.scalatest.{FlatSpec, Matchers}

class EqualQInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "EqualQInterpreter" should "test integers for equality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? 42 42>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "test multiple integers for equality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? 42 23 42 17>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "test integers for inequality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? 42 23>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "test multiple integers for inequality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? 42 23 <RTRUE> \"bad\">")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "test booleans for equality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? <RTRUE> <RTRUE>>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "test multiple booleans for equality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? <RTRUE> <RFALSE> <RTRUE> 42>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "test booleans for inequality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? <RTRUE> <RFALSE>>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "test multiple booleans for inequality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? <RTRUE> <RFALSE> <RFALSE> 42>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "test strings for equality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? \"abc\" \"abc\">")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "test multiple strings for equality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? \"abc\" \"bad\" \"abc\" 42>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(true)))
  }

  it should "test strings for inequality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? \"abc\" \"bad\">")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }

  it should "test multiple strings for inequality" in new Env {
    val i = Instruction.parser.parse("<EQUAL? \"abc\" \"bad\" 42 <RTRUE>>")
    Interpreter.evaluate(ctx)(i)
    ctx.pop should be(Some(BoolValue(false)))
  }
}
