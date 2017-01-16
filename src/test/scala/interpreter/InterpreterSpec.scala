package interpreter

import models.{DoubleValue, IntValue, Variable}
import org.scalatest.{FlatSpec, Matchers}

class InterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()
  }

  "Interpreter" should "evaluate int variables" in new Env {
    val intVar = Variable("42")
    val c = Interpreter.evaluate(ctx)(intVar)
    c.pop should be(Some(IntValue(42)))
  }

  it should "evaluate double variables" in new Env {
    val dVar = Variable("3.14")
    val c = Interpreter.evaluate(ctx)(dVar)
    c.pop should be(Some(DoubleValue(3.14)))
  }
}
