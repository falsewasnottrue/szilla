package interpreter.impl

import interpreter.Context
import models._

object TellInterpreter extends BaseInterpreter {
  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val args = arguments(ctx)(instruction, ValueTypes.arbitrary)
    args.foreach(value => ctx.out(value.toString))
    ctx
  }
}
