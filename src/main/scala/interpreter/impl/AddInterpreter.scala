package interpreter.impl

import interpreter.Context
import models._

object AddInterpreter extends InsInterpreter {

  override def apply(ctx: Context)(instruction: Instruction): Context = {
    val args = arguments(ctx)(instruction, ValueTypes.continually(IntType))
    val res = args.foldLeft(0) {
      case (acc, IntValue(i)) => acc + i
    }
    ctx.push(IntValue(res))
  }
}
