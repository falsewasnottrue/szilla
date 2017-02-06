package interpreter

import models._
import org.scalatest.{FlatSpec, Matchers}

class InterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = new Context()
  }

  "Interpreter" should "evaluate int variables" in new Env {
    val intVar = Variable("42")
    val c = Interpreter.evaluate(ctx)(intVar)
    c.pop should be(Some(IntValue(42)))
  }

  it should "evaluate string variables" in new Env {
    val sVar = Variable("this is a string")
    val c = Interpreter.evaluate(ctx)(sVar)
    c.pop should be(Some(StringValue("this is a string")))
  }

  it should "evaluate global variables" in new Env {
    val globalVar = GlobalVariable(",TEN")
    Global.define(globalVar, IntValue(10))

    val c = Interpreter.evaluate(ctx)(globalVar)
    c.pop should be(Some(IntValue(10)))
  }

  it should "fail if a variable is referenced that is not set" in new Env {
    val globalVar = Variable(",TEN")
    intercept[IllegalStateException] {
      Interpreter.evaluate(ctx)(globalVar)
    }
  }
}
