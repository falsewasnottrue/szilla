package interpreter.impl

import interpreter.{Context, Interpreter}
import models.Instruction
import org.scalatest.{FlatSpec, Matchers}

class BaseInterpreterSpec extends FlatSpec with Matchers {

  trait Env {
    val ctx = Context()

    def run(ctx: Context)(text: String): Context = {
      val instruction = Instruction.parser.parse(text)
      Interpreter.evaluate(ctx)(instruction)
    }
  }
}
