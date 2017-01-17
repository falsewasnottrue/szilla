package interpreter.impl

import interpreter.{Context, Interpreter}
import models.{Instruction, IntValue}

object RandomInterpreter extends Interpreter {

  override def apply(ctx: Context, i: Instruction): Context = {
    if (i.operands.size != 1) {
      throw new IllegalStateException(s"RANDOM needs exactly one ")
    }
    Interpreter.evaluate(ctx)(i.operands.head)
    ctx.pop match {
      case Some(IntValue(limit)) => {
        val rnd = (new java.util.Random().nextInt.abs % limit) + 1 // 1 <= r <= limit
        ctx.push(IntValue(rnd))
      }
      case x => throw new IllegalArgumentException(s"illegal argument for RANDOM $x")
    }

    ctx
  }
}
