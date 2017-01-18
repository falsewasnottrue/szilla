package interpreter.impl

import interpreter.Context
import models.{BoolValue, Instruction}

object CallInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = ???
}

// TODO RETURN

object RTrueInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(true))
  }
}

object RFalseInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(i: Instruction): Context = {
    arguments(ctx)(i, ValueTypes.empty)
    ctx.push(BoolValue(false))
  }
}