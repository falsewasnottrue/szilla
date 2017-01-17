package interpreter.impl

import interpreter.Context
import models.{BoolValue, Instruction, IntType, IntValue}

object ZeroQInterpreter extends InsInterpreter {

  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val Seq(IntValue(value)) = arguments(ctx)(instruction, ValueTypes(IntType))
    ctx.push(BoolValue(value == 0))
  }
}
