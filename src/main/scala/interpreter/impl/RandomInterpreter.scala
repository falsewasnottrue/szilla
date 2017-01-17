package interpreter.impl

import interpreter.Context
import models.{Instruction, IntType, IntValue}

object RandomInterpreter extends InsInterpreter {

  override def apply(ctx: Context)(i: Instruction): Context = {
    val Seq(IntValue(limit)) = arguments(ctx)(i, ValueTypes(IntType))
    val rnd = (new java.util.Random().nextInt.abs % limit) + 1 // 1 <= r <= limit
    ctx.push(IntValue(rnd))
  }
}
