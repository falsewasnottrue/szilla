package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, Object, ObjectValue, Variable}
import org.scalatest.{FlatSpec, Matchers}

class MoveInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()

    val breadVar = Variable(",BREAD")
    val toasterVar = Variable(",TOASTER")
    val bread = Object("bread")
    val toaster = Object("toaster")

    ctx.set(breadVar, ObjectValue(bread))
    ctx.set(toasterVar, ObjectValue(toaster))
  }

  "MoveInterpreter" should "put an object into another" in new Env {
    toaster.contains(bread) should be(false)
    val instruction = Instruction.parser.parse("<MOVE ,BREAD ,TOASTER>")
    Interpreter.evaluate(ctx)(instruction)

    toaster.contains(bread) should be(true)
  }

  it should "fail with too many parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<MOVE ,BREAD ,TOASTER 1>"))
    }
  }

  it should "fail with too few parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<MOVE ,BREAD>"))
    }
  }

  it should "fail with wrong type of parameters" in new Env {
    intercept[IllegalArgumentException] {
      Interpreter.evaluate(ctx)(Instruction.parser.parse("<MOVE ,BREAD \"bad\">"))
    }
  }
}
