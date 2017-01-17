package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{BoolValue, Instruction, IntValue}

object ZeroQInterpreter extends Interpreter {
  override def apply(ctx: Context, i: Instruction): Context = {
    if (i.operands.size != 1) {
      throw new IllegalArgumentException(s"ZERO? needs exactly one argument")
    }
    Interpreter.evaluate(ctx)(i.operands.head)
    ctx.pop match {
      case Some(IntValue(v)) => ctx.push(BoolValue(v == 0))
      case x => throw new IllegalArgumentException(s"wrong arguement for ZERO? $x")
    }

    ctx
  }
}
