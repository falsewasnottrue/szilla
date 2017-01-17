package interpreter.impl

import interpreter.Context
import models.{BoolValue, Instruction}

// TODO CALL
// TODO RETURN

object RTrueInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(true))
  }
}

object RFalseInterpreter extends InsInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(false))
  }
}